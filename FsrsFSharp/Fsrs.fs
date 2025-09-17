namespace Fsrs

open System

type Rating = | Again | Hard | Good | Easy

module Rating =
    let toValue = function | Again -> 1 | Hard -> 2 | Good -> 3 | Easy -> 4

type State = | Learning | Review | Relearning

type ReviewedCard = {
    State: State
    Step: int
    Stability: float
    Difficulty: float
}

type CardPhase =
    | New
    | Reviewed of ReviewedCard

type Card = {
    CardId: int64
    Interval: TimeSpan
    Phase: CardPhase
}

type SchedulerConfig = {
    Parameters: float[]
    DesiredRetention: float
    LearningSteps: TimeSpan[]
    RelearningSteps: TimeSpan[]
    MaximumInterval: int
    EnableFuzzing: bool
}

type Scheduler = private {
    Config: SchedulerConfig
    Parameters: float[]
    Decay: float
    Factor: float
}

module internal FsrsAlgorithm =
    let private minDifficulty = 1.0
    let private maxDifficulty = 10.0
    let private stabilityMin = 0.001

    let private clampDifficulty difficulty = difficulty |> max minDifficulty |> min maxDifficulty
    let private clampStability stability = max stability stabilityMin

    let private rawInitialDifficulty (parameters: float[]) (rating: Rating) =
        let ratingValue = rating |> Rating.toValue |> float
        parameters.[4] - (exp (parameters.[5] * (ratingValue - 1.0))) + 1.0

    let initialStability (parameters: float[]) (rating: Rating) =
        parameters.[(Rating.toValue rating) - 1] |> clampStability

    let initialDifficulty (parameters: float[]) (rating: Rating) =
        rawInitialDifficulty parameters rating |> clampDifficulty

    let nextInterval (factor: float) (retention: float) (decay: float)
                     (maxInterval: int) (stability: float) =
        (stability / factor) * (retention ** (1.0 / decay) - 1.0)
        |> round |> int |> max 1 |> min maxInterval
        |> float |> TimeSpan.FromDays

    let shortTermStability (parameters: float[]) (stability: float) (rating: Rating) =
        let ratingValue = rating |> Rating.toValue |> float
        let increase =
            exp (parameters.[17] * (ratingValue - 3.0 + parameters.[18]))
            * (stability ** -parameters.[19])
        let finalIncrease = match rating with Good | Easy -> max increase 1.0 | _ -> increase
        stability * finalIncrease |> clampStability

    let nextDifficulty (parameters: float[]) (difficulty: float) (rating: Rating) =
        let ratingValue = rating |> Rating.toValue |> float
        let deltaDifficulty = -(parameters.[6] * (ratingValue - 3.0))
        let dampedDelta =
            (maxDifficulty - difficulty) * deltaDifficulty / (maxDifficulty - minDifficulty)
        let initialEasyDifficulty = rawInitialDifficulty parameters Easy
        parameters.[7] * initialEasyDifficulty
        + (1.0 - parameters.[7]) * (difficulty + dampedDelta)
        |> clampDifficulty

    let private nextForgetStability (parameters: float[]) (data: ReviewedCard)
                                    (retrievability: float) =
        let p = parameters
        let longTerm =
            p.[11] * (data.Difficulty ** -p.[12])
            * (((data.Stability + 1.0) ** p.[13]) - 1.0)
            * (exp ((1.0 - retrievability) * p.[14]))
        let shortTerm = data.Stability / (exp (p.[17] * p.[18]))
        min longTerm shortTerm

    let private nextRecallStability (parameters: float[]) (data: ReviewedCard)
                                    (retrievability: float) (rating: Rating) =
        let p = parameters
        let hardPenalty = if rating = Hard then p.[15] else 1.0
        let easyBonus = if rating = Easy then p.[16] else 1.0
        data.Stability
        * (1.0 + exp p.[8] * (11.0 - data.Difficulty) * (data.Stability ** -p.[9])
        * (exp ((1.0 - retrievability) * p.[10]) - 1.0) * hardPenalty * easyBonus)

    let nextStability (parameters: float[]) (data: ReviewedCard)
                      (retrievability: float) (rating: Rating) =
        match rating with
        | Again -> nextForgetStability parameters data retrievability
        | _ -> nextRecallStability parameters data retrievability rating
        |> clampStability

    type private FuzzRange = { Start: float; End: float; Factor: float }
    let getFuzzedInterval (rand: Random) (maxInterval: int) (interval: TimeSpan) =
        let fuzzRanges = [
            { Start = 2.5; End = 7.0; Factor = 0.15 }
            { Start = 7.0; End = 20.0; Factor = 0.1 }
            { Start = 20.0; End = Double.PositiveInfinity; Factor = 0.05 }
        ]
        let intervalDays = interval.TotalDays
        if intervalDays < 2.5 then interval
        else
            let delta =
                fuzzRanges
                |> List.fold (fun acc range ->
                    acc + range.Factor * max 0.0 (min intervalDays range.End - range.Start)) 0.0
            let minIvl = intervalDays - delta |> round |> int |> max 2
            let maxIvl = intervalDays + delta |> round |> int
            rand.Next(minIvl, maxIvl + 1) |> min maxInterval |> float |> TimeSpan.FromDays

module Scheduler =
    open FsrsAlgorithm

    let DefaultConfig = {
        Parameters = [| 0.212; 1.2931; 2.3065; 8.2956; 6.4133; 0.8334; 3.0194; 0.001; 1.8722; 0.1666; 0.796; 1.4835; 0.0614; 0.2629; 1.6483; 0.6014; 1.8729; 0.5425; 0.0912; 0.0658; 0.1542 |]
        DesiredRetention = 0.9
        LearningSteps = [| TimeSpan.FromMinutes 1.0; TimeSpan.FromMinutes 10.0 |]
        RelearningSteps = [| TimeSpan.FromMinutes 10.0 |]
        MaximumInterval = 36500
        EnableFuzzing = true
    }

    let private checkAndFillParameters (parameters: float[]) =
        let fsrs5DefaultDecay = 0.5
        let filled =
            match parameters.Length with
            | 17 ->
                let p = Array.copy parameters
                p.[4] <- p.[5] * 2.0 + p.[4]
                p.[5] <- log (p.[5] * 3.0 + 1.0) / 3.0
                p.[6] <- p.[6] + 0.5
                Array.concat [| p; [| 0.0; 0.0; 0.0; fsrs5DefaultDecay |] |]
            | 19 -> Array.concat [| parameters; [| 0.0; fsrs5DefaultDecay |] |]
            | 21 -> parameters
            | _ -> raise (ArgumentException("Invalid number of parameters. Supported: 17, 19, or 21."))

        if filled |> Array.exists (Double.IsFinite >> not) then
            raise (ArgumentException("Invalid parameters: contains non-finite values."))

        filled

    let create (config: SchedulerConfig) : Scheduler =
        let filledParams = checkAndFillParameters config.Parameters
        let decay = -filledParams.[20]
        let scheduler = {
            Config = config
            Parameters = filledParams
            Decay = decay
            Factor = 0.9 ** (1.0 / decay) - 1.0
        }
        scheduler

    let internal calculateNextReviewInterval (scheduler: Scheduler) (stability: float) =
        nextInterval
            scheduler.Factor
            scheduler.Config.DesiredRetention
            scheduler.Decay
            scheduler.Config.MaximumInterval
            stability

    let private calculateNextReviewedState (scheduler: Scheduler) (card: Card)
                                           (rating: Rating) (reviewInterval: TimeSpan) =
        let getCardRetrievability (cardData: ReviewedCard) =
            let elapsedDays = max 0.0 reviewInterval.TotalDays
            (1.0 + scheduler.Factor * elapsedDays / cardData.Stability) ** scheduler.Decay

        let (stability, difficulty) =
            match card.Phase with
            | New ->
                initialStability scheduler.Parameters rating,
                initialDifficulty scheduler.Parameters rating
            | Reviewed data ->
                let newDifficulty = nextDifficulty scheduler.Parameters data.Difficulty rating
                let newStability =
                    if reviewInterval.TotalDays < 1.0 then
                        shortTermStability scheduler.Parameters data.Stability rating
                    else
                        let retrievability = getCardRetrievability data
                        nextStability scheduler.Parameters data retrievability rating
                (newStability, newDifficulty)

        let state, step = match card.Phase with | New -> Learning, 0 | Reviewed data -> data.State, data.Step
        { State = state; Step = step; Stability = stability; Difficulty = difficulty }

    let private toReviewState (scheduler: Scheduler) (reviewed: ReviewedCard) =
        let interval = calculateNextReviewInterval scheduler reviewed.Stability
        { reviewed with State = Review; Step = 0 }, interval

    let private hardIntervalStep (currentStep: int) (steps: TimeSpan[]) =
        match (currentStep, steps) with
        | (0, [| step1 |]) -> step1.TotalMinutes * 1.5 |> TimeSpan.FromMinutes
        | (0, [| step1; step2 |]) -> (step1.TotalMinutes + step2.TotalMinutes) / 2.0 |> TimeSpan.FromMinutes
        | _ -> steps.[currentStep]

    let private handleSteps (scheduler: Scheduler) (reviewed: ReviewedCard)
                            (rating: Rating) (steps: TimeSpan[]) =
        if Array.isEmpty steps then
            toReviewState scheduler reviewed
        else
            match rating with
            | Again -> { reviewed with State = Learning; Step = 0 }, steps.[0]
            | Hard ->
                let interval = hardIntervalStep reviewed.Step steps
                { reviewed with State = reviewed.State }, interval
            | Good ->
                let nextStep = reviewed.Step + 1
                if nextStep >= steps.Length then
                    toReviewState scheduler reviewed
                else
                    { reviewed with State = Learning; Step = nextStep }, steps.[nextStep]
            | Easy -> toReviewState scheduler reviewed

    let private determineNextPhaseAndInterval (scheduler: Scheduler) (reviewed: ReviewedCard)
                                              (rating: Rating) =
        let doHandleSteps = handleSteps scheduler reviewed rating

        match reviewed.State with
        | Learning -> doHandleSteps scheduler.Config.LearningSteps
        | Relearning -> doHandleSteps scheduler.Config.RelearningSteps
        | Review ->
            if rating = Again && not (Array.isEmpty scheduler.Config.RelearningSteps) then
                { reviewed with State = Relearning; Step = 0 }, scheduler.Config.RelearningSteps.[0]
            else
                toReviewState scheduler reviewed

    let private applyFuzzing (rand: Random) (scheduler: Scheduler)
                             (reviewed: ReviewedCard) (interval: TimeSpan) =
        match scheduler.Config.EnableFuzzing, reviewed.State with
        | true, Review -> getFuzzedInterval rand scheduler.Config.MaximumInterval interval
        | _ -> interval

    let reviewCard (rand: Random) (scheduler: Scheduler) (card: Card)
                   (rating: Rating) (reviewInterval: TimeSpan) =
        let nextReviewedState = calculateNextReviewedState scheduler card rating reviewInterval
        let (finalReviewedState, baseInterval) =
            determineNextPhaseAndInterval scheduler nextReviewedState rating
        let finalInterval = applyFuzzing rand scheduler finalReviewedState baseInterval
        let updatedCard = { card with Interval = finalInterval; Phase = Reviewed finalReviewedState }
        updatedCard

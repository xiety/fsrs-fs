namespace Fsrs.Tests

open Microsoft.VisualStudio.TestTools.UnitTesting
open Fsrs
open System
open FsUnit.MsTest

[<TestClass>]
type BasicTests () =

    let getReviewedData card =
        match card.Phase with
        | Reviewed data -> data
        | New -> failwith "Expected card to be in a Reviewed state, but it was New."

    let createTestCard() =
        { CardId = 1L; Interval = TimeSpan.Zero; Phase = New }

    let createDefaultScheduler() =
        Scheduler.create Scheduler.DefaultConfig

    let runReviews (rand: Random) (scheduler: Scheduler) (reviews: (Rating * int)[]) =
        let initialCard = createTestCard()
        reviews |> Array.fold (fun card (rating, interval) ->
            Scheduler.reviewCard rand scheduler card rating (TimeSpan.FromDays interval))
            initialCard

    let checkStabilityAndDifficulty (expectedStability: float) (expectedDifficulty: float) (card: Card) =
        let data = getReviewedData card
        data.Stability |> should (equalWithin 1e-4) expectedStability
        data.Difficulty |> should (equalWithin 1e-4) expectedDifficulty

    let changeCard (interval: int) (stability: float) (difficulty: float) (card: Card) =
        match card.Phase with
        | Reviewed reviewed ->
            { card with
                Interval = TimeSpan.FromDays(float interval)
                Phase = Reviewed { reviewed with Stability = stability; Difficulty = difficulty } }
        | _ -> failwith "Cannot modify a card that is not in the Reviewed phase."

    let rand = Random()

    [<TestMethod>]
    member _.TestNextInterval() =
        let desiredRetentions = [| 1..10 |] |> Array.map (float >> fun x -> x / 10.0)
        let config = { Scheduler.DefaultConfig with LearningSteps = [||]; EnableFuzzing = false; MaximumInterval = Int32.MaxValue }

        let actual = desiredRetentions |> Array.map (fun r ->
            let scheduler = Scheduler.create { config with DesiredRetention = r }
            let interval = Scheduler.calculateNextReviewInterval scheduler 1.0
            interval.TotalDays |> int
        )

        actual |> should equal [| 3116769; 34793; 2508; 387; 90; 27; 9; 3; 1; 1 |]

    [<TestMethod>]
    member _.TestFsrs() =
        let config = { Scheduler.DefaultConfig with LearningSteps = [||]; RelearningSteps = [||]; EnableFuzzing = false }
        let scheduler = Scheduler.create config
        let initialCard = createTestCard()
        let ratings = [| Rating.Again; Rating.Good; Rating.Good; Rating.Good; Rating.Good; Rating.Good |]

        let _, actualIntervals =
            ratings
            |> Array.fold (fun (card, intervals) rating ->
                let updatedCard = Scheduler.reviewCard rand scheduler card rating card.Interval
                let newIntervals = intervals @ [updatedCard.Interval.TotalDays |> int]
                (updatedCard, newIntervals)) (initialCard, [])

        actualIntervals |> should equal [ 1; 2; 6; 17; 44; 102 ]

    [<TestMethod>]
    member _.TestMemoState() =
        let parameters = [| 0.6845422; 1.6790825; 4.7349424; 10.042885; 7.4410233; 0.64219797; 1.071918; 0.0025195254; 1.432437; 0.1544; 0.8692766; 2.0696752; 0.0953; 0.2975; 2.4691248; 0.19542035; 3.201072; 0.18046261; 0.121442534 |]
        let scheduler = Scheduler.create { Scheduler.DefaultConfig with Parameters = parameters }
        let reviews = [| (Rating.Again, 0); (Rating.Good, 1); (Rating.Good, 3); (Rating.Good, 8); (Rating.Good, 21) |]

        let finalCard1 = runReviews rand scheduler reviews
        finalCard1 |> checkStabilityAndDifficulty 31.722992 7.382128

        let cardMod = finalCard1 |> changeCard 21 20.925528 7.005062
        let finalCard2 = Scheduler.reviewCard rand scheduler cardMod Rating.Good cardMod.Interval

        finalCard2 |> checkStabilityAndDifficulty 40.87456 6.9913807

    [<TestMethod>]
    member _.TestMemoryState() =
        let scheduler = createDefaultScheduler()
        let reviews = [| (Rating.Again, 0); (Rating.Good, 0); (Rating.Good, 1); (Rating.Good, 3); (Rating.Good, 8); (Rating.Good, 21) |]

        let finalCard1 = runReviews rand scheduler reviews
        finalCard1 |> checkStabilityAndDifficulty 53.62691 6.3574867

        let parameters2 = Array.copy Scheduler.DefaultConfig.Parameters
        for i in [17..19] do parameters2.[i] <- 0.0
        let scheduler2 = Scheduler.create { Scheduler.DefaultConfig with Parameters = parameters2 }

        let finalCard2 = runReviews rand scheduler2 reviews
        finalCard2 |> checkStabilityAndDifficulty 53.335106 6.3574867

    [<TestMethod>]
    member _.TestGoodLearningSteps() =
        let scheduler = createDefaultScheduler()
        let card = createTestCard()

        let cardAfterGood1 = Scheduler.reviewCard rand scheduler card Rating.Good card.Interval
        let data1 = getReviewedData cardAfterGood1
        data1.State |> should equal Learning
        data1.Step |> should equal 1
        cardAfterGood1.Interval.TotalMinutes |> should (equalWithin (1.0/60.0)) 10.0

        let cardAfterGood2 = Scheduler.reviewCard rand scheduler cardAfterGood1 Rating.Good cardAfterGood1.Interval
        let data2 = getReviewedData cardAfterGood2
        data2.State |> should equal Review
        cardAfterGood2.Interval.TotalDays |> should be (greaterThanOrEqualTo 1.0)

    [<TestMethod>]
    member _.TestAgainLearningSteps() =
        let scheduler = createDefaultScheduler()
        let card = createTestCard()
        let cardAfterAgain = Scheduler.reviewCard rand scheduler card Rating.Again card.Interval
        let data = getReviewedData cardAfterAgain
        data.State |> should equal Learning
        data.Step |> should equal 0
        cardAfterAgain.Interval.TotalMinutes |> should (equalWithin (1.0/60.0)) 1.0

    [<TestMethod>]
    member _.TestLearningCardRateHardOneLearningStep() =
        let config = { Scheduler.DefaultConfig with LearningSteps = [| TimeSpan.FromMinutes 10.0 |] }
        let scheduler = Scheduler.create config
        let card = createTestCard()
        let cardAfterHard = Scheduler.reviewCard rand scheduler card Rating.Hard card.Interval

        let expectedInterval = TimeSpan.FromMinutes(10.0 * 1.5)
        (abs (cardAfterHard.Interval - expectedInterval).TotalSeconds) |> should be (lessThanOrEqualTo 1.0)

    [<TestMethod>]
    member _.TestNoLearningSteps() =
        let config = { Scheduler.DefaultConfig with LearningSteps = Array.empty }
        let scheduler = Scheduler.create config
        let card = createTestCard()
        let updatedCard = Scheduler.reviewCard rand scheduler card Rating.Again card.Interval
        let updatedData = getReviewedData updatedCard
        updatedData.State |> should equal Review
        updatedCard.Interval.TotalDays |> should be (greaterThanOrEqualTo 1.0)

    [<TestMethod>]
    member _.TestMaximumInterval() =
        let config = { Scheduler.DefaultConfig with MaximumInterval = 100 }
        let scheduler = Scheduler.create config
        let card = createTestCard()

        let finalCard =
            Seq.init 10 (fun _ -> ())
            |> Seq.fold (fun currentCard _ ->
                Scheduler.reviewCard rand scheduler currentCard Rating.Easy currentCard.Interval) card

        finalCard.Interval.Days |> should be (lessThanOrEqualTo config.MaximumInterval)

    [<TestMethod>]
    member _.TestStabilityLowerBound() =
        let scheduler = createDefaultScheduler()
        let stabilityMin = 0.001
        let card = createTestCard()

        Seq.init 100 (fun _ -> ())
        |> Seq.fold (fun currentCard _ ->
            let nextReviewTime = currentCard.Interval.Add(TimeSpan.FromDays 1.0)
            let updatedCard = Scheduler.reviewCard rand scheduler currentCard Rating.Again nextReviewTime
            let updatedData = getReviewedData updatedCard
            updatedData.Stability |> should be (greaterThanOrEqualTo stabilityMin)
            updatedCard) card
        |> ignore

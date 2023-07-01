module Tests

open Swensen.Unquote
open FsCheck
open FsCheck.Xunit
open Fsia.Tennis
open Game

[<Property>]
let ``When deuce, the score updates correctly`` (playerThatScored: Player) =
    let actual = updateScoreWhenDeuce playerThatScored

    test <@ actual = Advantage playerThatScored @>

[<Property>]
let ``Given advantage, when advantaged player scores, the score is correct``
    (advantagedPlayer: Player)
    =
    let actual = updateScoreWhenAdvantage advantagedPlayer advantagedPlayer

    test
        <@
            actual = Game {
                PlayerThatWon = advantagedPlayer
                OtherPlayerPoint = Forty
            }
        @>

[<Property>]
let ``Given advantage, when other player than advantagedPlayer scores, the score is correct``
    (advantagedPlayer: Player)
    =
    let otherPlayer = otherPlayer advantagedPlayer
    let actual = updateScoreWhenAdvantage advantagedPlayer otherPlayer

    test <@ actual = Deuce @>

[<Property>]
let ``Given GamePoint, when player with GamePoint scores, the score is correct``
    (
        playerThatScored: Player,
        otherPlayerPoint: Point.Point
    ) =
    let actual =
        updateScoreWhenGamePoint
            playerThatScored
            playerThatScored
            otherPlayerPoint

    test
        <@
            actual = Game {
                PlayerThatWon = playerThatScored
                OtherPlayerPoint = Point otherPlayerPoint
            }
        @>

[<Property>]
let ``Given player: 40 - other: 30, when other player scores, the score is correct``
    (playerThatScored: Player)
    =
    let playerWithGamePoint = otherPlayer playerThatScored

    let actual =
        updateScoreWhenGamePoint
            playerWithGamePoint
            playerThatScored
            Point.Thirty

    test <@ actual = Deuce @>

[<Property>]
let ``Given player: 40 - other: < 30, when other player scores, the score is correct``
    (playerThatScored: Player)
    =
    let playerWithGamePoint = otherPlayer playerThatScored

    let otherPlayerPointGen =
        Gen.elements [ Point.Love; Point.Fifteen ]
        |> Arb.fromGen

    Prop.forAll otherPlayerPointGen (fun playerThatScoredPoint ->
        let actual =
            updateScoreWhenGamePoint
                playerWithGamePoint
                playerThatScored
                playerThatScoredPoint

        let expected =
            Point.increment playerThatScoredPoint
            |> Option.map (fun incrementedPlayerThatScoredPoint ->
                GamePoint {
                    PlayerWithGamePoint = playerWithGamePoint
                    OtherPlayerPoint = incrementedPlayerThatScoredPoint
                })

        test <@ Some actual = expected @>)

[<Property>]
let ``updateScore function handles all input``
    (
        gameScore: Game.Score,
        playerThatScored: Player
    ) =
    // Sanity check that the updateScore function does not crash
    updateScore gameScore playerThatScored
    |> ignore

let isGame =
    function
    | Game _ -> true
    | _ -> false

let isDeuce =
    function
    | Deuce -> true
    | _ -> false

let isAdvantage =
    function
    | Advantage _ -> true
    | _ -> false

let isPoints =
    function
    | Points _ -> true
    | _ -> false

let isGamePoint =
    function
    | GamePoint _ -> true
    | _ -> false

[<Property>]
let ``A game with less that 4 wins is not over``
    (playersThatScored: Player list)
    =
    let actual =
        playersThatScored
        |> Seq.truncate 3
        |> updateScoreForSeq newGame

    test <@ not (isGame actual) @>

[<Property>]
let ``A game with less than 6 wins is not deuce``
    (playersThatScored: Player list)
    =
    let actual =
        playersThatScored
        |> Seq.truncate 5
        |> updateScoreForSeq newGame

    test <@ not (isDeuce actual) @>

[<Property>]
let ``A game with less than 7 balls cannot have a player with advantage``
    (playersThatScored: Player list)
    =
    let actual =
        playersThatScored
        |> Seq.truncate 6
        |> updateScoreForSeq newGame

    test <@ not (isAdvantage actual) @>

let genMoreThanElements<'a> n =
    let generator = Arb.generate<'a>
    let baseList = Gen.listOfLength (n + 1) generator
    let additionalList = Gen.listOf generator
    Gen.map2 List.append baseList additionalList

[<Property>]
let ``A game with more that 4 balls cannot be a points score`` () =
    let moreThan4Balls = genMoreThanElements 4 |> Arb.fromGen

    Prop.forAll moreThan4Balls (fun (playersThatScored: Player list) ->
        let actual =
            playersThatScored
            |> updateScoreForSeq newGame

        test <@ not (isPoints actual) @>)

[<Property>]
let ``A game with more than 5 balls cannot be at Gamepoint`` () =
    let moreThan5Balls = genMoreThanElements 5 |> Arb.fromGen

    Prop.forAll moreThan5Balls (fun (playersThatScored: Player list) ->
        let actual =
            playersThatScored
            |> updateScoreForSeq newGame

        test <@ not (isGamePoint actual) @>)

[<Property>]
let ``A game where only one player scores is won in 4 balls``
    (playerThatScores: Player)
    =
    let actual =
        Seq.init 4 (fun _ -> playerThatScores)
        |> updateScoreForSeq newGame

    let expected =
        Game {
            PlayerThatWon = playerThatScores
            OtherPlayerPoint = Point Point.Love
        }

    test <@ actual = expected @>

[<Property>]
let ``A game with alternate scoring players never ends`` player =
    let alternatingWins =
        player
        |> Gen.constant
        |> Gen.map (fun p -> [ p; otherPlayer p ])
        |> Gen.listOf
        |> Gen.map List.concat
        |> Arb.fromGen

    Prop.forAll alternatingWins (fun playersThatScored ->
        let actual =
            playersThatScored
            |> updateScoreForSeq newGame

        test <@ not (isGame actual) @>)

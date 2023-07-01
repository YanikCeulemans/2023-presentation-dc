module Tests

open Swensen.Unquote
open FsCheck
open FsCheck.Xunit
open Fsia.Tennis

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

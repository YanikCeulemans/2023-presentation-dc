module Tests

open FsCheck.FSharp
open FsCheck.Xunit
open Fsia.Tennis
open Swensen.Unquote

[<Property>]
let ``When deuce, the score updates correctly`` (playerThatScored: Player) =
    let actual = Game.updateWhenDeuce playerThatScored

    test
        <@
            actual = Game.GameScore.NormalGameScore(
                NormalGame.Advantage playerThatScored
            )
        @>

[<Property>]
let ``Given advantage, when advantaged player scores, the score is correct``
    (advantagedPlayer: Player)
    =
    let actual = Game.updateScoreAdvantage advantagedPlayer advantagedPlayer

    test
        <@
            actual = Game.NormalGameWon(
                advantagedPlayer,
                NormalGame.LoserScore.Forty
            )
        @>

[<Property>]
let ``Given advantage, when other player than advantagedPlayer scores, the score is correct``
    (advantagedPlayer: Player)
    =
    let otherPlayer = Game.otherPlayer advantagedPlayer
    let actual = Game.updateScoreAdvantage advantagedPlayer otherPlayer

    test <@ actual = Game.NormalGameScore NormalGame.Deuce @>

[<Property>]
let ``Given GamePoint, when player with GamePoint scores, the score is correct``
    (
        playerThatScored: Player,
        otherPlayerPoint: NormalGame.Point.Point
    ) =
    let actual =
        Game.updateScoreWhenGamePoint
            playerThatScored
            playerThatScored
            otherPlayerPoint

    let loserScorePoint = NormalGame.LoserScore.Normal otherPlayerPoint

    test <@ actual = Game.NormalGameWon(playerThatScored, loserScorePoint) @>

[<Property>]
let ``Given player: 40 - other: 30, when other player scores, the score is correct``
    (otherPlayer: Player)
    =
    let player = Game.otherPlayer otherPlayer

    let actual =
        Game.updateScoreWhenGamePoint player otherPlayer NormalGame.Point.Thirty

    test <@ actual = Game.NormalGameScore NormalGame.Deuce @>

[<Property>]
let ``Given player: 40 - other: < 30, when other player scores, the score is correct``
    (otherPlayer: Player)
    =
    let player = Game.otherPlayer otherPlayer

    let otherPlayerPointGen =
        Gen.elements [ NormalGame.Point.Love; NormalGame.Point.Fifteen ]
        |> Arb.fromGen

    Prop.forAll otherPlayerPointGen (fun otherPlayerPoint ->
        let actual =
            Game.updateScoreWhenGamePoint player otherPlayer otherPlayerPoint

        let expected =
            NormalGame.Point.increment otherPlayerPoint
            |> Option.map (fun newOtherPlayerPoint ->
                Game.NormalGameScore(
                    NormalGame.GamePoint {
                        Player = player
                        OtherPlayerPoint = newOtherPlayerPoint
                    }
                ))

        test <@ Some actual = expected @>)

[<Property>]
let ``updateScore function handles all input``
    (
        gameScore: Game.GameScore,
        playerThatScored: Player
    ) =
    // Sanity check that the updateScore function does not crash
    Game.updateScore gameScore playerThatScored
    |> ignore

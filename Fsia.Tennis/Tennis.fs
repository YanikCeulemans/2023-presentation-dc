namespace Fsia.Tennis

type Player =
    | PlayerOne
    | PlayerTwo

module NormalGame =
    module Point =
        type Point =
            | Love
            | Fifteen
            | Thirty

        // How would you do this in C#?
        let increment point =
            match point with
            | Love -> Some Fifteen
            | Fifteen -> Some Thirty
            | Thirty -> None

    open Point

    type NormalScore = {
        PlayerOne: Point
        PlayerTwo: Point
    }

    type GamePointScore = {
        Player: Player
        OtherPlayerPoint: Point
    }

    type Score =
        | NormalGame of NormalScore
        | GamePoint of GamePointScore
        | Deuce
        | Advantage of Player

    module LoserScore =
        type Score =
            | Normal of Point
            | Forty

module GameMsg =
    type Msg = PointScored of Player

module Game =
    open NormalGame
    open NormalGame.Point
    open GameMsg

    type GameScore =
        | NormalGameScore of Score
        | NormalGameWon of Player * LoserScore.Score

    let otherPlayer =
        function
        | PlayerOne -> PlayerTwo
        | PlayerTwo -> PlayerOne

    let updateWhenDeuce playerThatScored =
        NormalGameScore(Advantage playerThatScored)

    let updateScoreAdvantage advantagePlayer playerThatScored =
        if advantagePlayer = playerThatScored then
            NormalGameWon(advantagePlayer, LoserScore.Forty)
        else
            NormalGameScore Deuce

    let updateScoreWhenGamePoint
        gamePointPlayer
        playerThatScored
        otherPlayerPoint
        =
        if gamePointPlayer = playerThatScored then
            NormalGameWon(gamePointPlayer, LoserScore.Normal otherPlayerPoint)
        else
            match increment otherPlayerPoint with
            | Some newPoint ->
                NormalGameScore(
                    GamePoint {
                        Player = gamePointPlayer
                        OtherPlayerPoint = newPoint
                    }
                )
            | None -> NormalGameScore Deuce

    let updateScoreWhenNormal playerThatScored normalScore =
        if playerThatScored = PlayerOne then
            match increment normalScore.PlayerOne with
            | Some incrementedPlayerOnePoint ->
                NormalGameScore(
                    NormalGame {
                        normalScore with
                            PlayerOne = incrementedPlayerOnePoint
                    }
                )
            | None ->
                NormalGameScore(
                    GamePoint {
                        Player = playerThatScored
                        OtherPlayerPoint = normalScore.PlayerTwo
                    }
                )
        else
            match increment normalScore.PlayerTwo with
            | Some incrementedPlayerTwoPoint ->
                NormalGameScore(
                    NormalGame {
                        normalScore with
                            PlayerTwo = incrementedPlayerTwoPoint
                    }
                )
            | None ->
                NormalGameScore(
                    GamePoint {
                        Player = playerThatScored
                        OtherPlayerPoint = normalScore.PlayerOne
                    }
                )

    let updateScore score playerThatScored =
        match score with
        | NormalGameScore(NormalGame normalScore) ->
            updateScoreWhenNormal playerThatScored normalScore

        | NormalGameScore(GamePoint gamePointScore) ->
            updateScoreWhenGamePoint
                gamePointScore.Player
                playerThatScored
                gamePointScore.OtherPlayerPoint

        | NormalGameScore(Deuce) -> updateWhenDeuce playerThatScored

        | NormalGameScore(Advantage advantagePlayer) ->
            updateScoreAdvantage advantagePlayer playerThatScored

        | NormalGameWon(playerThatWon, otherPlayerScore) ->
            NormalGameWon(playerThatWon, otherPlayerScore)


    let updateGame msg score =
        match msg with
        | PointScored playerThatScored -> updateScore score playerThatScored

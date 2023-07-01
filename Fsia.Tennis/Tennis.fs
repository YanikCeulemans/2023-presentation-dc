module Fsia.Tennis

type Player =
    | PlayerOne
    | PlayerTwo

let otherPlayer player =
    match player with
    | PlayerOne -> PlayerTwo
    | PlayerTwo -> PlayerOne

module Game =
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

    type PointsScore = {
        PlayerOne: Point
        PlayerTwo: Point
    }

    module PointsScore =
        let pointForPlayer pointsScore player =
            match player with
            | PlayerOne -> pointsScore.PlayerOne
            | PlayerTwo -> pointsScore.PlayerTwo

        let updatePlayerPoint pointsScore player point =
            match player with
            | PlayerOne -> {
                pointsScore with
                    PlayerOne = point
              }
            | PlayerTwo -> {
                pointsScore with
                    PlayerTwo = point
              }

    type GamePointScore = {
        PlayerWithGamePoint: Player
        OtherPlayerPoint: Point
    }

    type GameWonOtherPoint =
        | Point of Point
        | Forty

    type GameScore = {
        PlayerThatWon: Player
        OtherPlayerPoint: GameWonOtherPoint
    }

    type Score =
        | Points of PointsScore
        | GamePoint of GamePointScore
        | Deuce
        | Advantage of Player
        | Game of GameScore

    let newGame = Points {
        PlayerOne = Love
        PlayerTwo = Love
    }

    let updateScoreWhenDeuce playerThatScored = Advantage playerThatScored

    let updateScoreWhenAdvantage playerWithAdvantage playerThatScored =
        if playerWithAdvantage = playerThatScored then
            Game {
                PlayerThatWon = playerWithAdvantage
                OtherPlayerPoint = Forty
            }
        else
            Deuce

    let updateScoreWhenGamePoint
        playerWithGamePoint
        playerThatScored
        otherPlayerPoint
        =
        if playerWithGamePoint = playerThatScored then
            Game {
                PlayerThatWon = playerWithGamePoint
                OtherPlayerPoint = Point otherPlayerPoint
            }
        else
            match increment otherPlayerPoint with
            | Some incrementedOtherPlayerPoint ->
                GamePoint {
                    PlayerWithGamePoint = playerWithGamePoint
                    OtherPlayerPoint = incrementedOtherPlayerPoint
                }

            | None -> Deuce

    let updateScoreWhenPoints playerThatScored points =
        let playerThatScoredPoint =
            PointsScore.pointForPlayer points playerThatScored

        match increment playerThatScoredPoint with
        | Some incrementedPoint ->
            PointsScore.updatePlayerPoint
                points
                playerThatScored
                incrementedPoint
            |> Points

        | None ->
            GamePoint {
                PlayerWithGamePoint = playerThatScored
                OtherPlayerPoint =
                    PointsScore.pointForPlayer
                        points
                        (otherPlayer playerThatScored)
            }

    let updateScore score playerThatScored =
        match score with
        | Game gameScore -> Game gameScore

        | Deuce -> updateScoreWhenDeuce playerThatScored

        | Advantage playerWithAdvantage ->
            updateScoreWhenAdvantage playerWithAdvantage playerThatScored

        | GamePoint gamePointScore ->
            updateScoreWhenGamePoint
                gamePointScore.PlayerWithGamePoint
                playerThatScored
                gamePointScore.OtherPlayerPoint

        | Points points -> updateScoreWhenPoints playerThatScored points

    let updateScoreForSeq score playersThatScored =
        Seq.fold updateScore score playersThatScored
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
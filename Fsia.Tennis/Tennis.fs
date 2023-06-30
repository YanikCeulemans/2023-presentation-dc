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

    // TODO: can we simplify this? Using helper function such as OtherPlayerPoint for example?
    let updateScoreWhenPoints playerThatScored points =
        match playerThatScored with
        | PlayerOne ->
            match increment points.PlayerOne with
            | Some incrementedPlayerOnePoint ->
                Points { points with PlayerOne = incrementedPlayerOnePoint }

            | None ->
                GamePoint { PlayerWithGamePoint = PlayerOne; OtherPlayerPoint = points.PlayerTwo }

        | PlayerTwo ->
            match increment points.PlayerTwo with
            | Some incrementedPlayerTwoPoint ->
                Points { points with PlayerTwo = incrementedPlayerTwoPoint }
            
            | None ->
                GamePoint { PlayerWithGamePoint = PlayerTwo; OtherPlayerPoint = points.PlayerOne }


    let updateScore score playerThatScored =
        match score with
        | Game gameScore ->
            Game gameScore

        | Deuce -> updateScoreWhenDeuce playerThatScored

        | Advantage playerWithAdvantage ->
            updateScoreWhenAdvantage playerWithAdvantage playerThatScored

        | GamePoint gamePointScore -> 
            updateScoreWhenGamePoint gamePointScore.PlayerWithGamePoint playerThatScored gamePointScore.OtherPlayerPoint

        | Points points -> 
            updateScoreWhenPoints playerThatScored points

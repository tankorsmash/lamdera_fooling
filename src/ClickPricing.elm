module ClickPricing exposing (..)

import Duration
import Time


{-| 0 to 100, for the sake of animations
-}
type Progress
    = NotStarted
    | Progress Int
    | Completed


addToProgress : Progress -> Int -> Progress
addToProgress originalProgress toAdd =
    case originalProgress of
        NotStarted ->
            if toAdd >= 100 then
                Completed

            else
                Progress toAdd

        Progress progress ->
            if (progress + toAdd |> min 100 |> max 0) >= 100 then
                Completed

            else
                Progress (progress + toAdd |> min 100 |> max 0)

        Completed ->
            originalProgress


createProgress : Int -> Progress
createProgress value =
    if value == 0 then
        NotStarted

    else if (value |> min 100 |> max 0) >= 100 then
        Completed

    else
        Progress (value |> min 100 |> max 0)


type Level
    = Level Int


addLevel : Level -> Level -> Level
addLevel (Level left) (Level right) =
    Level <| left + right


addToLevel : Level -> Int -> Level
addToLevel (Level level) toAdd =
    Level (level + toAdd)


getLevel : Level -> Int
getLevel (Level level) =
    level


nextLevel : Level -> Level
nextLevel (Level level) =
    Level <| level + 1


getNextLevel : Level -> Int
getNextLevel (Level level) =
    level + 1


groupMemberClickBonus : Int -> Int
groupMemberClickBonus members =
    members
        |> max 0


contributeClickBonus : Level -> Int
contributeClickBonus (Level level) =
    level


type Bonus
    = Bonus
        { clickBonus : Level -> Int
        , xpCost : Level -> Int
        , durationMs : Level -> Duration.Duration
        }


type alias Bonuses =
    { discuss : Bonus
    , argue : Bonus
    }


basicBonuses : Bonuses
basicBonuses =
    { discuss =
        Bonus
            { clickBonus = \(Level level) -> level + 5
            , xpCost = \(Level level) -> level * 5
            , durationMs = \(Level level) -> Duration.seconds 10
            }
    , argue =
        Bonus
            { clickBonus = \(Level level) -> level * 5
            , xpCost = \(Level level) -> level * 45
            , durationMs = \(Level level) -> Duration.seconds 30
            }
    }


clickBonus : Bonus -> Level -> Int
clickBonus (Bonus bonus) level =
    bonus.clickBonus level


xpCost : Bonus -> Level -> Int
xpCost (Bonus bonus) level =
    bonus.xpCost level


bonusDuration : Bonus -> Level -> Duration.Duration
bonusDuration (Bonus bonus) level =
    bonus.durationMs level


type CurrentLevel
    = CurrentLevel Level (Maybe ( Time.Posix, Time.Posix ))


type alias CurrentLevels =
    { discuss : CurrentLevel
    , argue : CurrentLevel
    }


mapCurrentLevels : (CurrentLevels -> CurrentLevel) -> (CurrentLevels -> CurrentLevel -> CurrentLevels) -> CurrentLevels -> CurrentLevels
mapCurrentLevels getter setter currentLevels =
    let
        currentLevel =
            getter currentLevels
    in
    setter currentLevels currentLevel


mapCurrentLevel : CurrentLevel -> (Level -> Maybe ( Time.Posix, Time.Posix ) -> CurrentLevel) -> CurrentLevel
mapCurrentLevel (CurrentLevel level maybeTimes) mapper =
    mapper level maybeTimes


restartCurrentLevel : CurrentLevel -> Time.Posix -> Duration.Duration -> CurrentLevel
restartCurrentLevel (CurrentLevel level _) now duration =
    CurrentLevel level (Just ( now, Duration.addTo now duration ))


getCurrentLevelLevel : CurrentLevel -> Level
getCurrentLevelLevel (CurrentLevel level _) =
    level


getCurrentLevelProgress : CurrentLevel -> Time.Posix -> Progress
getCurrentLevelProgress (CurrentLevel level maybeTimes) now =
    case maybeTimes of
        Just ( startTime, endTime ) ->
            let
                startTimeMs =
                    Time.posixToMillis startTime

                endTimeMs =
                    Time.posixToMillis endTime

                nowMs =
                    Time.posixToMillis now
            in
            normalizeFloat (toFloat startTimeMs) (toFloat endTimeMs) (toFloat nowMs)
                |> round
                |> createProgress

        Nothing ->
            NotStarted


normalizeInt : Int -> Int -> Int -> Int
normalizeInt min max val =
    let
        range =
            max - min

        diff =
            val - min
    in
    diff // range


normalizeFloat : Float -> Float -> Float -> Float
normalizeFloat min max val =
    let
        range =
            max - min

        diff =
            val - min
    in
    diff / range


setCurrentLevelLevel : CurrentLevel -> Level -> CurrentLevel
setCurrentLevelLevel (CurrentLevel level maybeTimes) newLevel =
    CurrentLevel newLevel maybeTimes



-- doesnt make sense to have anymore, since we're now using times
-- setCurrentLevelProgress : CurrentLevel -> Progress -> CurrentLevel
-- setCurrentLevelProgress (CurrentLevel level progress durationMs) newProgress =
--     CurrentLevel level newProgress durationMs

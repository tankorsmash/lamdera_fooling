module ClickPricing exposing (..)

import Duration
import Time


{-| 0.0 to 1.0. A 0 to 100 helper is available
-}
type Progress
    = NotStarted
    | Progress Float
    | Completed


getProgressInt : Progress -> Int
getProgressInt progress =
    case progress of
        NotStarted ->
            0

        Progress raw ->
            raw * 100 |> round

        Completed ->
            100


getProgress : Progress -> Float
getProgress progress =
    case progress of
        NotStarted ->
            0

        Progress raw ->
            raw

        Completed ->
            1.0



-- addToProgress : Progress -> Int -> Progress
-- addToProgress originalProgress toAdd =
--     case originalProgress of
--         NotStarted ->
--             if toAdd >= 100 then
--                 Completed
--
--             else
--                 Progress toAdd
--
--         Progress progress ->
--             if (progress + toAdd |> min 100 |> max 0) >= 100 then
--                 Completed
--
--             else
--                 Progress (progress + toAdd |> min 100 |> max 0)
--
--         Completed ->
--             originalProgress


{-| only creates a started or completed one, if you want to make a non started one, use the ctor -}
createProgress : Float -> Progress
createProgress value =
    if (value |> min 1.0 |> max 0) >= 1.0 then
        Completed

    else
        Progress (value |> min 1.0 |> max 0)


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
    , energize : Bonus
    }


{-| global scaling for these Bonuses. you pass the CurrentLevels data into these
-}
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
            { clickBonus = \(Level level) -> (level * 5) + 30
            , xpCost = \(Level level) -> level * 45
            , durationMs = \(Level level) -> Duration.seconds 30
            }
    , energize =
        Bonus
            { clickBonus = \(Level level) -> (level ) 
            , xpCost = \(Level level) -> level * 25
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
    , energize : CurrentLevel
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

stopCurrentLevel : CurrentLevel ->  CurrentLevel
stopCurrentLevel (CurrentLevel level _) =
    CurrentLevel level (Nothing)


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

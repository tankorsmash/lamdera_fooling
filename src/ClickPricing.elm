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


{-| only creates a started or completed one, if you want to make a non started one, use the ctor
-}
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
    | CycleBonus
        { clickBonus : Level -> Int
        , xpCost : Level -> Int
        , durationMs : Level -> Duration.Duration
        , cycleCap : Level -> Int
        , cycleCapUpgradeCost : Level -> Int
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
            , durationMs = always <| Duration.seconds 10
            }
    , argue =
        Bonus
            { clickBonus = \(Level level) -> (level * 5) + 30
            , xpCost = \(Level level) -> level * 45
            , durationMs = always <| Duration.seconds 30
            }
    , energize =
        CycleBonus
            { clickBonus = \(Level level) -> level
            , xpCost = \(Level level) -> level * 25
            , durationMs = always <| Duration.seconds 45
            , cycleCap = \(Level level) -> level * 10
            , cycleCapUpgradeCost = \(Level level) -> level * 15
            }
    }


clickBonus : Bonus -> Level -> Int
clickBonus wholeBonus level =
    case wholeBonus of
        Bonus bonus ->
            bonus.clickBonus level

        CycleBonus bonus ->
            bonus.clickBonus level


xpCost : Bonus -> Level -> Int
xpCost wholeBonus level =
    case wholeBonus of
        Bonus bonus ->
            bonus.xpCost level

        CycleBonus bonus ->
            bonus.xpCost level


bonusDuration : Bonus -> Level -> Duration.Duration
bonusDuration wholeBonus level =
    case wholeBonus of
        Bonus bonus ->
            bonus.durationMs level

        CycleBonus bonus ->
            bonus.durationMs level


cycleCap : Bonus -> Level -> Maybe Int
cycleCap wholeBonus level =
    case wholeBonus of
        Bonus _ ->
            Nothing

        CycleBonus bonus ->
            Just <| bonus.cycleCap level

cycleCapUpgradeCost : Bonus -> Level -> Maybe Int
cycleCapUpgradeCost wholeBonus level =
    case wholeBonus of
        Bonus _ ->
            Nothing

        CycleBonus bonus ->
            Just <| bonus.cycleCapUpgradeCost level


type CurrentLevel
    = CurrentLevel Level (Maybe ( Time.Posix, Time.Posix ))


type alias CurrentLevels =
    { discuss : CurrentLevel
    , argue : CurrentLevel
    , energize : CurrentLevel
    , energizeCycleCap : CurrentLevel
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


restartCurrentLevelHelper : CurrentLevel -> Time.Posix -> Duration.Duration -> CurrentLevel
restartCurrentLevelHelper (CurrentLevel level _) now duration =
    CurrentLevel level (Just ( now, Duration.addTo now duration ))


currentLevelRestarter : CurrentLevel -> Time.Posix -> Bonus -> CurrentLevel
currentLevelRestarter currentLevel tick bonus =
    restartCurrentLevelHelper
        currentLevel
        tick
        (bonusDuration bonus <|
            getCurrentLevelLevel currentLevel
        )


stopCurrentLevel : CurrentLevel -> CurrentLevel
stopCurrentLevel (CurrentLevel level _) =
    CurrentLevel level Nothing


getCurrentLevelLevel : CurrentLevel -> Level
getCurrentLevelLevel (CurrentLevel level _) =
    level


{-| gets the progress between the start time, end time, and now
-}
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


{-| assumes the progress cycles in durations worth of loops
-}
getCurrentLevelCycleProgress : CurrentLevel -> Time.Posix -> Duration.Duration -> Progress
getCurrentLevelCycleProgress (CurrentLevel level maybeTimes) now duration =
    case maybeTimes of
        Just ( startTime, _ ) ->
            let
                startTimeMs =
                    Time.posixToMillis startTime

                nowMs =
                    Time.posixToMillis now

                elapsedMs =
                    nowMs - startTimeMs

                durationMs =
                    Duration.inMilliseconds duration

                currentLoopMaxMs =
                    modBy (round durationMs) elapsedMs
            in
            normalizeFloat 0 durationMs (toFloat <| currentLoopMaxMs)
                |> createProgress

        Nothing ->
            NotStarted


{-| get the number of cycles since the last collection
-}
getCurrentLevelCycleCount : CurrentLevel -> Time.Posix -> Duration.Duration -> Maybe Int
getCurrentLevelCycleCount (CurrentLevel level maybeTimes) now duration =
    case maybeTimes of
        Just ( startTime, lastCollectionTime ) ->
            let
                startTimeMs =
                    Time.posixToMillis startTime

                nowMs =
                    Time.posixToMillis now

                elapsedMs =
                    nowMs - startTimeMs

                durationMs =
                    Duration.inMilliseconds duration

                rawCycles =
                    floor <| toFloat elapsedMs / durationMs
            in
            Just <| rawCycles

        Nothing ->
            Nothing


{-| get the cycles available to be collected
-}
getAvailableCyclesCurrentLevel : CurrentLevel -> Time.Posix -> Duration.Duration -> Maybe Int
getAvailableCyclesCurrentLevel (CurrentLevel level maybeTimes) now duration =
    case maybeTimes of
        Just ( startTime, lastCollectionTime ) ->
            let
                nowMs =
                    Time.posixToMillis now

                elapsedSinceStartMs =
                    nowMs - Time.posixToMillis startTime

                elapsedSinceLastMs =
                    Time.posixToMillis lastCollectionTime - Time.posixToMillis startTime

                durationMs =
                    Duration.inMilliseconds duration

                rawCycles =
                    floor <| toFloat elapsedSinceStartMs / durationMs

                claimedCycles =
                    floor <| toFloat elapsedSinceLastMs / durationMs
            in
            Just <| rawCycles - claimedCycles

        Nothing ->
            Nothing


{-| update the lastCollectionTime and return the collected clicks
-}
collectCurrentLevel : CurrentLevel -> Time.Posix -> Duration.Duration -> Int -> ( CurrentLevel, Maybe Int )
collectCurrentLevel ((CurrentLevel level maybeTimes) as currentLevel) now duration cycleCap_ =
    case maybeTimes of
        Just ( startTime, _ ) ->
            let
                toCollect =
                    getAvailableCyclesCurrentLevel currentLevel now duration
                        |> --limit output
                           Maybe.map (min cycleCap_)
            in
            ( CurrentLevel level (Just ( startTime, now )), toCollect )

        Nothing ->
            ( CurrentLevel level Nothing, Nothing )


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

module ClickPricing exposing (..)

import Time


{-| 0 to 100, for the sake of animations
-}
type Progress
    = Progress Int


addToProgress : Progress -> Int -> Progress
addToProgress (Progress progress) toAdd =
    Progress (progress + toAdd |> min 100 |> max 0)


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
            }
    , argue =
        Bonus
            { clickBonus = \(Level level) -> level * 5
            , xpCost = \(Level level) -> level * 45
            }
    }


clickBonus : Bonus -> Level -> Int
clickBonus (Bonus bonus) level =
    bonus.clickBonus level


xpCost : Bonus -> Level -> Int
xpCost (Bonus bonus) level =
    bonus.xpCost level


type CurrentLevel
    = CurrentLevel Level (Maybe (Time.Posix, Time.Posix))


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


mapCurrentLevel : CurrentLevel -> (Level -> Maybe (Time.Posix, Time.Posix) -> CurrentLevel) -> CurrentLevel
mapCurrentLevel (CurrentLevel level maybeTimes) mapper =
    mapper level maybeTimes


getCurrentLevelLevel : CurrentLevel -> Level
getCurrentLevelLevel (CurrentLevel level _) =
    level


getCurrentLevelProgress : CurrentLevel -> Time.Posix -> Progress
getCurrentLevelProgress (CurrentLevel level maybeTimes ) now =
    case maybeTimes of
        Just (startTime, endTime) ->
            let
                startTimeMs = Time.posixToMillis startTime
                endTimeMs = Time.posixToMillis endTime
                nowMs= Time.posixToMillis now
            in
            normalizeFloat (toFloat startTimeMs) (toFloat endTimeMs) (toFloat nowMs)
            |> round
            |> Progress
        Nothing ->
            Progress 100

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

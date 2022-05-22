module ClickPricing exposing (..)

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
    = CurrentLevel Level Progress


type alias CurrentLevels =
    { discuss : CurrentLevel
    , argue : CurrentLevel
    }


mapCurrentLevels : CurrentLevels -> (CurrentLevels -> CurrentLevel) -> (CurrentLevels -> CurrentLevel -> CurrentLevels) -> CurrentLevels
mapCurrentLevels currentLevels getter setter =
    let
        currentLevel =
            getter currentLevels
    in
    setter currentLevels currentLevel


mapCurrentLevel : CurrentLevel -> (Level -> Progress -> CurrentLevel) -> CurrentLevel
mapCurrentLevel (CurrentLevel level progress) mapper =
    mapper level progress


getCurrentLevelLevel : CurrentLevel -> Level
getCurrentLevelLevel (CurrentLevel level progress) =
    level


getCurrentLevelProgress : CurrentLevel -> Progress
getCurrentLevelProgress (CurrentLevel level progress) =
    progress


setCurrentLevelLevel : CurrentLevel -> Level -> CurrentLevel
setCurrentLevelLevel (CurrentLevel level progress) newLevel =
    CurrentLevel newLevel progress


setCurrentLevelProgress : CurrentLevel -> Progress -> CurrentLevel
setCurrentLevelProgress (CurrentLevel level progress) newProgress =
    CurrentLevel level newProgress

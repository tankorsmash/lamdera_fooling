module ClickPricing exposing (..)


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


superContributeClickBonus : Level -> Int
superContributeClickBonus (Level level) =
    level + 5


selfImprovementXpCost : Level -> Int
selfImprovementXpCost (Level level) =
    level * 5

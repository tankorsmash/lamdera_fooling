module ClickPricing exposing (..)


type Level
    = Level Int


addLevel : Level -> Level -> Level
addLevel (Level left) (Level right) =
    Level <| left + right


addToLevel : Level -> Int -> Level
addToLevel (Level level) toAdd =
    Level (level + toAdd)


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

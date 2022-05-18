module ClickPricing exposing (..)


groupMemberClickBonus : Int -> Int
groupMemberClickBonus members =
    members
        |> max 0

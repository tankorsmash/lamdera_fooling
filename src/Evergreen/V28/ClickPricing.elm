module Evergreen.V28.ClickPricing exposing (..)

import Time


type Level
    = Level Int


type CurrentLevel
    = CurrentLevel Level (Maybe (Time.Posix, Time.Posix))


type alias CurrentLevels = 
    { clickCap : CurrentLevel
    , discuss : CurrentLevel
    , argue : CurrentLevel
    , energize : CurrentLevel
    , energizeCycleCap : CurrentLevel
    }
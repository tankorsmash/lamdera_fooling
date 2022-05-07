module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { key : Key
    , message : String
    , clicksFromBackend : Int
    }


type alias BackendModel =
    { message : String
    , clicks : Int
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | SendClickToBackend


type ToBackend
    = NoOpToBackend
    | ToBackendClick


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int

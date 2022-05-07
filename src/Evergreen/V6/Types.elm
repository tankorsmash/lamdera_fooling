module Evergreen.V6.Types exposing (..)

import Browser
import Browser.Navigation
import Lamdera
import Url


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , message : String
    , clicksFromBackend : Int
    }


type alias BackendModel =
    { message : String
    , clicks : Int
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | SendClickToBackend


type ToBackend
    = NoOpToBackend
    | ToBackendClick


type BackendMsg
    = NoOpBackendMsg
    | OnClientConnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int

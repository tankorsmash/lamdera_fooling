module Types exposing (PersonalityType(..), BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend(..), User(..))

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type PersonalityType
    = Idealistic
    | Realistic


type User
    = --user on home screen
      AnonymousUser (Maybe PersonalityType)
      -- user choosing a side
    | PreppingUser
      -- chosen side and logged in
    | PreppedUser


type alias FrontendModel =
    { key : Key
    , message : String
    , clicksFromBackend : Int
    , user : User
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
    | OnClientConnect SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int

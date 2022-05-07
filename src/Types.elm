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
    | PreppingUser ClientId
      -- chosen side and logged in
    | PreppedUser ClientId


type alias FrontendModel =
    { key : Key
    , message : String
    , clicksFromBackend : Int
    , user : User
    }


type alias BackendModel =
    { message : String
    , clicks : Int
    , users : List User
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | SendClickToBackend
    | TryingOutPersonalityType (Maybe PersonalityType)
    | ResetPersonalityType
    | ConfirmedPersonalityType PersonalityType


type ToBackend
    = NoOpToBackend
    | ToBackendClick
    | UserChoseToBe PersonalityType


type BackendMsg
    = NoOpBackendMsg
    | OnClientConnect SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int

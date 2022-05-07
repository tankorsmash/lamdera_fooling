module Types exposing (getClientId, BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), PersonalityType(..), ToBackend(..), ToFrontend(..), User(..))

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
    | PreppingUser ClientId PersonalityType
      -- chosen side and logged in
    | FullUser ClientId PersonalityType


{-| get client id from user, if possible
-}
getClientId : User -> Maybe ClientId
getClientId user =
    case user of
        AnonymousUser _ ->
            Nothing

        PreppingUser clientId _ ->
            Just clientId

        FullUser clientId _ ->
            Just clientId


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
    | NewUser User

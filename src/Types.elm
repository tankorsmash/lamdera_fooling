module Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), PersonalityType(..), ToBackend(..), ToFrontend(..), User(..), getSessionId)

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
    | PreppingUser SessionId PersonalityType
      -- chosen side and logged in
    | FullUser SessionId PersonalityType


{-| get client id from user, if possible
-}
getSessionId : User -> Maybe SessionId
getSessionId user =
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
    , totalUsers : Int
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
      -- onboarding
    | TryingOutPersonalityType (Maybe PersonalityType)
    | ResetPersonalityType
    | ConfirmedPersonalityType PersonalityType
    | FinalizeUser


type ToBackend
    = NoOpToBackend
    | ToBackendClick
    | UserChoseToBe PersonalityType
    | UserFinalizedUser


type BackendMsg
    = NoOpBackendMsg
    | OnClientConnect SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int
    | NewUser User
    | NewTotalUsers Int

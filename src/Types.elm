module Types exposing (mapUserData, BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), PersonalityType(..), ToBackend(..), ToFrontend(..), User(..), getSessionId)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type PersonalityType
    = Idealistic
    | Realistic


type alias UserData =
    { personalityType : PersonalityType
    , sessionId : Maybe SessionId
    , username : String
    }


type User
    = --user on home screen
      AnonymousUser (Maybe PersonalityType)
      -- user choosing a side
    | PreppingUser SessionId PersonalityType
      -- chosen side and logged in
    | FullUser UserData


{-| get username from user, if possible
-}
getUsername : User -> Maybe String
getUsername user =
    case user of
        AnonymousUser _ ->
            Nothing

        PreppingUser _ _ ->
            Nothing

        FullUser userData ->
            Just userData.username


mapUserData : User -> (UserData -> a) -> Maybe a
mapUserData user mapper =
    case user of
        AnonymousUser _ ->
            Nothing

        PreppingUser _ _ ->
            Nothing

        FullUser userData ->
            Just (mapper userData)


{-| get session id from user, if possible
-}
getSessionId : User -> Maybe SessionId
getSessionId user =
    case user of
        AnonymousUser _ ->
            Nothing

        PreppingUser sessionId _ ->
            Just sessionId

        FullUser userData ->
            userData.sessionId


type alias FrontendModel =
    { key : Key
    , message : String
    , clicksFromBackend : Int
    , username : String
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
    | ChangedUsername String
    | FinalizeUser


type ToBackend
    = NoOpToBackend
    | ToBackendClick
    | UserChoseToBe PersonalityType
    | UserFinalizedUser String


type BackendMsg
    = NoOpBackendMsg
    | OnClientConnect SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int
    | NewUser User
    | NewTotalUsers Int

module Types exposing (BackendModel, BackendMsg(..), FrontendModel, FrontendMsg(..), PersonalityType(..), ToBackend(..), ToFrontend(..), User(..), getSessionId, getUsername, initBackendModel, initFrontendModel, mapUserData, personalityTypeToDataId, setUserData, stringToPersonalityType)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Url exposing (Url)


type PersonalityType
    = Idealistic
    | Realistic


type alias UserData =
    { personalityType : PersonalityType
    , sessionId : Maybe SessionId
    , username : String
    , userClicks : Int
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


setUserData : User -> UserData -> User
setUserData user newUserData =
    case user of
        AnonymousUser _ ->
            user

        PreppingUser _ _ ->
            user

        FullUser userData ->
            FullUser newUserData


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


initFrontendModel : Key -> FrontendModel
initFrontendModel key =
    { key = key
    , message = "Now this is different"
    , totalClicksFromBackend = 0
    , personalityTypeClicksFromBackend = Dict.empty
    , userClicksFromBackend = 0
    , user = AnonymousUser Nothing
    , username = ""

    -- , user = AnonymousUser (Just Idealistic)
    -- , user = AnonymousUser (Just Realistic)
    , totalUsers = 0
    , ignoreme = -1
    }


type alias FrontendModel =
    { key : Key
    , message : String
    , totalClicksFromBackend : Int
    , personalityTypeClicksFromBackend : Dict.Dict PersonalityTypeDataId Int
    , userClicksFromBackend : Int
    , username : String
    , user : User
    , totalUsers : Int
    , ignoreme : Int
    }


type alias PersonalityTypeDataId =
    String


personalityTypeToDataId : PersonalityType -> String
personalityTypeToDataId personalityType =
    case personalityType of
        Idealistic ->
            "Idealistic"

        Realistic ->
            "Realistic"


stringToPersonalityType : String -> Maybe PersonalityType
stringToPersonalityType strType =
    case strType of
        "Idealistic" ->
            Just Idealistic

        "Realistic" ->
            Just Realistic

        _ ->
            Nothing


initBackendModel : BackendModel
initBackendModel =
    { message = "Hello!"
    , totalClicks = 0
    , clicksByPersonalityType =
        Dict.fromList
            [ ( "Idealistic", 0 )
            , ( "Realistic", 0 )
            ]
    , users = []
    , ignoreme2 = -1
    }


type alias BackendModel =
    { message : String
    , totalClicks : Int
    , clicksByPersonalityType : Dict.Dict PersonalityTypeDataId Int
    , users : List User
    , ignoreme2 : Int
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
      -- playing messages
    | LogUserOut


type ToBackend
    = NoOpToBackend
    | UserGainedAClick
    | UserChoseToBe PersonalityType
    | UserFinalizedUser String
    | UserLoggedOut


type BackendMsg
    = NoOpBackendMsg
    | OnClientConnect SessionId ClientId


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int
    | NewClicksByPersonalityType (Dict.Dict PersonalityTypeDataId Int)
    | NewUser User
    | NewTotalUsers Int
    | NewClicksByUser Int

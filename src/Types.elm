module Types exposing (BackendModel, BackendMsg(..), ChatMessage, FrontendModel, FrontendMsg(..), PersonalityType(..), PersonalityTypeDict, ToBackend(..), ToFrontend(..), User(..), UserData, getSessionId, getUserData, getUsername, initBackendModel, initFrontendModel, mapFullUser, mapPreppingUser, mapUserData, personalityTypeToDataId, setUserData, stringToPersonalityType)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import Time
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


mapPreppingUser : (SessionId -> PersonalityType -> User) -> User -> User
mapPreppingUser callback user =
    case user of
        AnonymousUser _ ->
            user

        PreppingUser sessionId personalityType ->
            callback sessionId personalityType

        FullUser _ ->
            user


mapFullUser : (UserData -> User) -> User -> User
mapFullUser callback user =
    case user of
        AnonymousUser _ ->
            user

        PreppingUser _ _ ->
            user

        FullUser userData ->
            callback userData


getUserData : User -> Maybe UserData
getUserData user =
    mapUserData user identity


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
    , newUsername = ""

    -- , user = AnonymousUser (Just Idealistic)
    -- , user = AnonymousUser (Just Realistic)
    , totalUsers = 0
    , usernamesByPersonalityTypes = Dict.empty
    , userChatMessage = Nothing
    , allChatMessages = []
    }


type alias ChatMessage =
    { userData : UserData
    , message : String
    , date : String
    }


type alias FrontendModel =
    { key : Key
    , message : String
    , totalClicksFromBackend : Int
    , personalityTypeClicksFromBackend : Dict.Dict PersonalityTypeDataId Int
    , userClicksFromBackend : Int
    , newUsername : String
    , user : User
    , totalUsers : Int
    , usernamesByPersonalityTypes : PersonalityTypeDict (List ( String, Int ))
    , userChatMessage : Maybe String
    , allChatMessages : List ChatMessage
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
    , allChatMessages = []
    }


type alias BackendModel =
    { message : String
    , totalClicks : Int
    , clicksByPersonalityType : Dict.Dict PersonalityTypeDataId Int
    , users : List User
    , allChatMessages : List ChatMessage
    }


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
      -- onboarding
    | TryingOutPersonalityType (Maybe PersonalityType)
    | ResetPersonalityType
    | ConfirmedPersonalityType PersonalityType
    | ChangedUsername String
    | FinalizeUser
      -- playing messages
    | LogUserOut
    | SendClickToBackend
    | SendWantsToSpendToBackend
      -- chat messages
    | ChatInputChanged (Maybe String)
    | ChatInputSent
    | FocusError (Result Browser.Dom.Error ())


type ToBackend
    = NoOpToBackend
    | UserGainedAClick
    | UserWantsToSpend
    | UserChoseToBe PersonalityType
    | UserFinalizedUser String
    | UserLoggedOut
    | UserSentMessage String


type BackendMsg
    = NoOpBackendMsg
    | OnClientConnect SessionId ClientId
    | UpdateTick Time.Posix


type alias PersonalityTypeDict a =
    Dict.Dict PersonalityTypeDataId a


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int
    | NewClicksByPersonalityType (PersonalityTypeDict Int)
    | NewUser User
    | NewTotalUsers Int
    | NewClicksByUser Int
    | NewUsernamesByPersonalityTypes (PersonalityTypeDict (List ( String, Int )))
    | NewTick Time.Posix
    | NewAllChatMessages (List ChatMessage)

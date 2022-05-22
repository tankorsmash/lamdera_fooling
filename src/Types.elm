module Types exposing (BackendModel, BackendMsg(..), ChatMessage, FrontendModel, FrontendMsg(..), Group, GroupId, PersonalityType(..), PersonalityTypeDict, Team, Teams, TeamsUserClicks, ToBackend(..), ToFrontend(..), Upgrade(..), UpgradeType(..), User(..), UserData, createUserData, generateUuid, getGroupNumGroupMembers, getSessionId, getTeamByPersonality, getUserData, getUserGroup, getUsername, initBackendModel, initFrontendModel, mapFullUser, mapPreppingUser, mapUserData, personalityTypeToDataId, setUserData, stringToPersonalityType)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import ClickPricing exposing (..)
import Dict exposing (Dict)
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Time
import UUID
import Url exposing (Url)


type PersonalityType
    = Idealistic
    | Realistic


type alias UserData =
    { personalityType : PersonalityType
    , sessionId : Maybe SessionId
    , username : String
    , userClicks : Int
    , isOnline : Bool
    , xp : Int
    , groupId : Maybe GroupId
    , userId : UUID.UUID
    , currentLevels : CurrentLevels
    }


createUserData : SessionId -> String -> PersonalityType -> UserData
createUserData sessionId username personalityType =
    { sessionId = Just sessionId
    , username = username
    , personalityType = personalityType
    , userClicks = 0
    , isOnline = True
    , xp = 0
    , groupId = Nothing
    , userId = generateUuid (username ++ sessionId)
    , currentLevels =
        { discuss = CurrentLevel (Level 0) Nothing 0
        , argue = CurrentLevel (Level 0) Nothing 0
        }
    }


type alias GroupId =
    UUID.UUID


type User
    = --user on home screen
      AnonymousUser (Maybe PersonalityType)
      -- user choosing a side
    | PreppingUser SessionId PersonalityType
      -- chosen side and logged in
    | FullUser UserData


getUserGroup : Teams -> UserData -> Maybe Group
getUserGroup teams userData =
    let
        team =
            getTeamByPersonality
                teams
                userData.personalityType

        getGroup : UUID.UUID -> Maybe Group
        getGroup groupId =
            List.Extra.find
                (.groupId >> (==) groupId)
                team.groups
    in
    Maybe.andThen getGroup userData.groupId


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
    , teamsFromBackend = initTeams
    , userClicksFromBackend = 0
    , user = AnonymousUser Nothing
    , newUsername = ""
    , totalUsers = 0
    , teamsUserClicks = { realists = [], idealists = [] }
    , userChatMessage = Nothing
    , allChatMessages = []
    , lastTick = Time.millisToPosix 0
    }


type alias ChatMessage =
    { userData : UserData
    , message : String
    , date : String
    }


type alias UserClickData =
    { username : String, clicks : Int, isOnline : Bool }


type alias TeamsUserClicks =
    { realists : List UserClickData, idealists : List UserClickData }


type alias FrontendModel =
    { key : Key
    , message : String
    , totalClicksFromBackend : Int
    , teamsFromBackend : Teams
    , userClicksFromBackend : Int
    , newUsername : String
    , user : User
    , totalUsers : Int
    , teamsUserClicks : TeamsUserClicks
    , userChatMessage : Maybe String
    , allChatMessages : List ChatMessage
    , lastTick : Time.Posix
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
    , teams = initTeams
    , users = []
    , allChatMessages = []
    }


type alias Team =
    { totalTeamClicks : Int
    , totalTeamPoints : Int
    , groups : List Group
    }


{-| NOTE: uuid is based on name, so dont create two groups with the same name
-}
createGroup : String -> Group
createGroup groupName =
    { name = groupName, members = [], groupId = generateUuid groupName }


generateUuid : String -> UUID.UUID
generateUuid str =
    UUID.forName str UUID.dnsNamespace


initTeams : Teams
initTeams =
    { realists =
        { totalTeamPoints = 0
        , totalTeamClicks = 0
        , groups = [ createGroup "The Straight Shooters", createGroup "Glasses Half Full" ]
        }
    , idealists =
        { totalTeamPoints = 0
        , totalTeamClicks = 0
        , groups = [ createGroup "With Eyes Wide Open", createGroup "Excited For The Future" ]
        }
    }


getTeamByPersonality : Teams -> PersonalityType -> Team
getTeamByPersonality teams personalityType =
    case personalityType of
        Realistic ->
            teams.realists

        Idealistic ->
            teams.idealists


type alias BackendModel =
    { message : String
    , totalClicks : Int
    , teams : Teams
    , users : List User
    , allChatMessages : List ChatMessage
    }


type Upgrade
    = Upgrade UpgradeType Int


type UpgradeType
    = SelfImprovement Level


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | LocalTick Time.Posix
      -- onboarding
    | TryingOutPersonalityType (Maybe PersonalityType)
    | ResetPersonalityType
    | ConfirmedPersonalityType PersonalityType
    | ChangedUsername String
    | FinalizeUser
      -- playing messages
    | LogUserOut
    | SendClickToBackend
    | Discuss
    | Argue
    | SendWantsToSpendToBackend
    | SendBuyUpgrade UpgradeType
    | TryToJoinGroup UUID.UUID
    | TryToLeaveGroup
      -- chat messages
    | ChatInputChanged (Maybe String)
    | ChatInputSent
    | FocusError (Result Browser.Dom.Error ())


type ToBackend
    = NoOpToBackend
    | UserGainedAClick
    | UserDiscussed
    | UserArgued
    | UserWantsToSpend
    | UserChoseToBe PersonalityType
    | UserFinalizedUser String
    | UserLoggedOut
    | UserSentMessage String
    | UserWantsToBuyUpgrade UpgradeType
    | UserWantsToJoinGroup UUID.UUID
    | UserWantsToLeaveGroup


type BackendMsg
    = NoOpBackendMsg
    | OnClientConnect SessionId ClientId
    | OnClientDisconnect SessionId ClientId
    | UpdateTick Time.Posix


type alias PersonalityTypeDict a =
    Dict.Dict PersonalityTypeDataId a


type alias Teams =
    { realists : Team, idealists : Team }


type alias UserId =
    UUID.UUID


type alias Group =
    { members : List UserId, name : String, groupId : UUID.UUID }


getGroupNumGroupMembers : Teams -> UserData -> Maybe Int
getGroupNumGroupMembers teams userData =
    getUserGroup teams userData
        |> Maybe.map (.members >> List.length)


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int
    | NewTeams Teams
    | NewUser User
    | NewTotalUsers Int
    | NewClicksByUser Int
    | NewUsernamesByPersonalityTypes TeamsUserClicks
    | NewTick Time.Posix
    | NewAllChatMessages (List ChatMessage)

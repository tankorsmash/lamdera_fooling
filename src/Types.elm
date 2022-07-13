module Types exposing (AdminFrontendModel, AdminFrontendMsg(..), AdminToBackend(..), BackendModel, BackendMsg(..), ChatMessage, ChatMessageId, CyclingTimeline, DashboardModel, DashboardMsg(..), DashboardTabType(..), DashboardToBackend(..), FrontendModel, FrontendMsg(..), FrontpageModel, FrontpageMsg(..), FrontpageToBackend(..), Group, GroupId, LabelValue(..), PersonalityType(..), PersonalityTypeDataId, PersonalityTypeDict, SignUpMsg(..), SignupModel, SignupPasswordData, SignupToBackend(..), Team, Teams, TeamsUserClicks, Timelines, ToAdminFrontend(..), ToBackend(..), ToFrontend(..), ToSignupFrontend(..), Upgrade(..), UpgradeType(..), User(..), UserClickData, UserData, UserId, adminSendToBackend, buildChatMessageUuuid, createGroup, createUserData, generateUuid, getGroupNumGroupMembers, getSessionId, getTeamByPersonality, getUserData, getUserGroup, getUsername, initAdminFrontendModel, initBackendModel, initDashboardModel, initFrontendModel, initFrontpageModel, initSignUpModel, initTeams, mapFullUser, mapPreppingUser, mapUserData, otherPersonalityType, personalityTypeToDataId, setAdminFrontendModel, setSignupModel, setUserData, stringToPersonalityType)

import Browser exposing (UrlRequest)
import Browser.Dom
import Browser.Navigation exposing (Key)
import ClickPricing exposing (..)
import Dict exposing (Dict)
import Element
import External.Animator.Animator as Animator
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Password
import Random
import Time
import UUID
import Url exposing (Url)


type PersonalityType
    = Idealistic
    | Realistic


otherPersonalityType : PersonalityType -> PersonalityType
otherPersonalityType personalityType =
    case personalityType of
        Idealistic ->
            Realistic

        Realistic ->
            Idealistic


type alias UserData =
    { personalityType : PersonalityType
    , sessionId : Maybe SessionId
    , username : String
    , password : Maybe Password.HashedPassword
    , userClicks : Int
    , isOnline : Bool
    , xp : Int
    , groupId : Maybe GroupId
    , userId : UUID.UUID
    , currentLevels : CurrentLevels
    }


createUserData : Maybe SessionId -> String -> PersonalityType -> UserData
createUserData maybeSessionId username personalityType =
    { sessionId = maybeSessionId
    , username = username
    , personalityType = personalityType
    , password = Nothing
    , userClicks = 0
    , isOnline = False
    , xp = 0
    , groupId = Nothing
    , userId =
        case maybeSessionId of
            Just sessionId ->
                generateUuid (username ++ sessionId)

            Nothing ->
                --hope this doesn't cause problems
                generateUuid username
    , currentLevels =
        { clickCap = CurrentLevel (Level 0) Nothing
        , discuss = CurrentLevel (Level 0) Nothing
        , argue = CurrentLevel (Level 0) Nothing
        , energize = CurrentLevel (Level 0) Nothing
        , energizeCycleCap = CurrentLevel (Level 0) Nothing
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


type LabelValue
    = LabelNumber Int
    | LabelNumber2 Int
    | LabelString String


type alias CyclingTimeline =
    ( Animator.Timeline Int, Int )


type alias Timelines =
    { userClicksTimeline : Animator.Timeline (Maybe LabelValue)
    , --This is a tuple because Animator.previous doesn't work, so we manually track it https://github.com/mdgriffith/elm-animator/issues/16
      cyclingNumberTimeline : CyclingTimeline
    }


initAdminFrontendModel : Url -> Key -> AdminFrontendModel
initAdminFrontendModel url key =
    { url = url
    , key = key
    , users = []
    , allChatMessages = []
    , selectedChatMessageUuid = Nothing
    }


initFrontendModel : Url -> Key -> FrontendModel
initFrontendModel url key =
    { key = key
    , url = url
    , device = Nothing
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
    , timelines =
        { userClicksTimeline = Animator.init Nothing
        , cyclingNumberTimeline = ( Animator.init 1, 0 )
        }
    , adminFrontendModel = initAdminFrontendModel url key
    , dashboardModel = initDashboardModel url key
    , frontpageModel = initFrontpageModel url key
    , signupModel = initSignUpModel url key
    }


initFrontpageModel : Url -> Key -> FrontpageModel
initFrontpageModel url key =
    { url = url, key = key }


initSignUpModel : Url -> Key -> SignupModel
initSignUpModel url key =
    { url = url
    , key = key
    , username = Nothing
    , password = Nothing
    , globalSeed = Random.initialSeed 12345
    , signupSubmitError = Nothing
    , personalityType = Realistic
    }


type alias ChatMessageId =
    UUID.UUID


type alias ChatMessage =
    { --TODO use userId instead of tracking the entire user
      userData : UserData
    , message : String
    , date : Time.Posix
    , uuid : ChatMessageId
    }


buildChatMessageUuuid : String -> String -> Time.Posix -> UUID.UUID
buildChatMessageUuuid username message timestamp =
    timestamp
        |> Time.posixToMillis
        |> String.fromInt
        |> (++) (message ++ username)
        |> generateUuid


type alias UserClickData =
    { username : String, clicks : Int, isOnline : Bool }


type alias TeamsUserClicks =
    { realists : List UserClickData, idealists : List UserClickData }


type alias DashboardModel =
    { key : Key
    , url : Url.Url
    , currentTabType : DashboardTabType
    }


initDashboardModel : Url -> Key -> DashboardModel
initDashboardModel url key =
    { url = url, key = key, currentTabType = DashboardActionsTabType }


type alias FrontendModel =
    { key : Key
    , url : Url.Url
    , device : Maybe Element.Device
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
    , timelines : Timelines
    , adminFrontendModel : AdminFrontendModel
    , dashboardModel : DashboardModel
    , frontpageModel : FrontpageModel
    , signupModel : SignupModel
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
    , lastTick = Time.millisToPosix 0
    , globalSeed = Random.initialSeed 0
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
    , lastTick : Time.Posix
    , globalSeed : Random.Seed
    }


type Upgrade
    = Upgrade UpgradeType Int


type UpgradeType
    = Discussion Level
    | Argumentation Level
    | Energization Level
    | EnergizeCap Level
    | ClickCap Level


setAdminFrontendModel : FrontendModel -> AdminFrontendModel -> FrontendModel
setAdminFrontendModel model adminFrontendModel =
    { model | adminFrontendModel = adminFrontendModel }


setSignupModel : FrontendModel -> SignupModel -> FrontendModel
setSignupModel model signupFrontendModel =
    { model | signupModel = signupFrontendModel }


type alias AdminFrontendModel =
    { url : Url.Url
    , key : Key
    , users : List User
    , allChatMessages : List ChatMessage
    , selectedChatMessageUuid : Maybe UUID.UUID
    }


type DashboardTabType
    = DashboardActionsTabType
    | DashboardUpgradesTabType
    | DashboardProfileTabType


type DashboardMsg
    = NoOpDashboardFrontend
    | ChangeTab DashboardTabType
    | LogUserOutFromDashboard


type DashboardToBackend
    = NoOpDashboardToBackend


type AdminFrontendMsg
    = NoOpAdminFrontend
    | DownloadUsers
    | DownloadAllChatMessages
    | AddDummyUsers Int
    | AddDummyChatMessages Int
    | SelectChatMessage (Maybe ChatMessageId)
    | DeleteSelectedMessage ChatMessageId


type AdminToBackend
    = NoOpAdminToBackend
    | AdminWantsToDownloadUsers
    | AdminWantsToDownloadChatMessages
    | AdminWantsToAddDummyUsers Int
    | AdminWantsToAddDummyChatMessages Int
    | AdminWantsToDeleteChatMessage ChatMessageId


type FrontendMsg
    = UrlClicked UrlRequest
    | UrlChanged Url
    | NoOpFrontendMsg
    | LocalTick Time.Posix
    | OnWindowResize Int Int
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
    | CollectEnergize
    | SendWantsToSpendToBackend
    | SendBuyUpgrade UpgradeType
    | TryToJoinGroup UUID.UUID
    | TryToLeaveGroup
      -- chat messages
    | ChatInputChanged (Maybe String)
    | ChatInputSent
    | FocusError (Result Browser.Dom.Error ())
    | GotAdminFrontendMsg AdminFrontendMsg
    | GotPlayerDashboardMsg DashboardMsg
    | GotFrontpageMsg FrontpageMsg
    | GotSignupMsg SignUpMsg
    | SendWantsToCraftXp Int


type ToBackend
    = NoOpToBackend
    | UserGainedAClick
    | UserDiscussed
    | UserArgued
    | UserEnergized
    | UserWantsToSpend
    | UserChoseToBe PersonalityType
    | UserFinalizedUser String
    | UserLoggedOut
    | UserSentMessage String
    | UserWantsToBuyUpgrade UpgradeType
    | UserWantsToJoinGroup UUID.UUID
    | UserWantsToLeaveGroup
    | UserWantsToCraftXp Int
    | AdminSendingToBackend AdminToBackend
    | SignupSendingToBackend SignupToBackend


type BackendMsg
    = NoOpBackendMsg
    | OnClientConnect SessionId ClientId
    | OnClientDisconnect SessionId ClientId
    | UpdateTick Time.Posix


type alias FrontpageModel =
    { url : Url.Url, key : Browser.Navigation.Key }


type FrontpageMsg
    = NoOpFrontpage


type FrontpageToBackend
    = NoOpFrontpageToBackend


type alias SignupPasswordData =
    { rawPassword : String
    , --this would be a zxcvbn strength value, but i dont want to depend on it if i can
      passwordStrength : Int
    }


type alias SignupModel =
    { url : Url.Url
    , key : Browser.Navigation.Key
    , username : Maybe String
    , password : Maybe SignupPasswordData
    , personalityType : PersonalityType
    , globalSeed : Random.Seed
    , signupSubmitError : Maybe String
    }


type SignUpMsg
    = NoOpSignUp
    | SignUpUsernameChanged String
    | SignUpPasswordChanged String
    | SignUpPersonalitySelected PersonalityType
    | SignUpSubmit


type SignupToBackend
    = NoOpSignUpToBackend
    | SignupNewUserToBackend { username : String, hashedPassword : Password.HashedPassword }


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


{-| Allows the AdminPage to send to backend
-}
adminSendToBackend : AdminToBackend -> Cmd msg
adminSendToBackend adminToBackend_ =
    Lamdera.sendToBackend (AdminSendingToBackend adminToBackend_)


type ToAdminFrontend
    = NoOpToAdminFrontend
    | DownloadedUsers (List User)
    | DownloadedChatMessages (List ChatMessage)


type ToSignupFrontend
    = NoOpToSignupFrontend
    | SignupRejectedUserExists
    | SignupAccepted User


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
    | NewToAdminFrontend ToAdminFrontend
    | NewToSignupFrontend ToSignupFrontend

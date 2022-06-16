module Evergreen.V30.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Evergreen.V30.ClickPricing
import Evergreen.V30.External.Animator.Animator
import Lamdera
import Time
import UUID
import Url


type alias UserId = UUID.UUID


type alias Group = 
    { members : (List UserId)
    , name : String
    , groupId : UUID.UUID
    }


type alias Team = 
    { totalTeamClicks : Int
    , totalTeamPoints : Int
    , groups : (List Group)
    }


type alias Teams = 
    { realists : Team
    , idealists : Team
    }


type PersonalityType
    = Idealistic
    | Realistic


type alias GroupId = UUID.UUID


type alias UserData = 
    { personalityType : PersonalityType
    , sessionId : (Maybe Lamdera.SessionId)
    , username : String
    , userClicks : Int
    , isOnline : Bool
    , xp : Int
    , groupId : (Maybe GroupId)
    , userId : UUID.UUID
    , currentLevels : Evergreen.V30.ClickPricing.CurrentLevels
    }


type User
    = AnonymousUser (Maybe PersonalityType)
    | PreppingUser Lamdera.SessionId PersonalityType
    | FullUser UserData


type alias UserClickData = 
    { username : String
    , clicks : Int
    , isOnline : Bool
    }


type alias TeamsUserClicks = 
    { realists : (List UserClickData)
    , idealists : (List UserClickData)
    }


type alias ChatMessage = 
    { userData : UserData
    , message : String
    , date : String
    }


type LabelValue
    = LabelNumber Int
    | LabelNumber2 Int
    | LabelString String


type alias CyclingTimeline = ((Evergreen.V30.External.Animator.Animator.Timeline Int), Int)


type alias Timelines = 
    { userClicksTimeline : (Evergreen.V30.External.Animator.Animator.Timeline (Maybe LabelValue))
    , cyclingNumberTimeline : CyclingTimeline
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , url : Url.Url
    , message : String
    , totalClicksFromBackend : Int
    , teamsFromBackend : Teams
    , userClicksFromBackend : Int
    , newUsername : String
    , user : User
    , totalUsers : Int
    , teamsUserClicks : TeamsUserClicks
    , userChatMessage : (Maybe String)
    , allChatMessages : (List ChatMessage)
    , lastTick : Time.Posix
    , timelines : Timelines
    }


type alias BackendModel =
    { message : String
    , totalClicks : Int
    , teams : Teams
    , users : (List User)
    , allChatMessages : (List ChatMessage)
    , lastTick : Time.Posix
    }


type UpgradeType
    = Discussion Evergreen.V30.ClickPricing.Level
    | Argumentation Evergreen.V30.ClickPricing.Level
    | Energization Evergreen.V30.ClickPricing.Level
    | EnergizeCap Evergreen.V30.ClickPricing.Level
    | ClickCap Evergreen.V30.ClickPricing.Level


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | LocalTick Time.Posix
    | TryingOutPersonalityType (Maybe PersonalityType)
    | ResetPersonalityType
    | ConfirmedPersonalityType PersonalityType
    | ChangedUsername String
    | FinalizeUser
    | LogUserOut
    | SendClickToBackend
    | Discuss
    | Argue
    | CollectEnergize
    | SendWantsToSpendToBackend
    | SendBuyUpgrade UpgradeType
    | TryToJoinGroup UUID.UUID
    | TryToLeaveGroup
    | ChatInputChanged (Maybe String)
    | ChatInputSent
    | FocusError (Result Browser.Dom.Error ())


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


type BackendMsg
    = NoOpBackendMsg
    | OnClientConnect Lamdera.SessionId Lamdera.ClientId
    | OnClientDisconnect Lamdera.SessionId Lamdera.ClientId
    | UpdateTick Time.Posix


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
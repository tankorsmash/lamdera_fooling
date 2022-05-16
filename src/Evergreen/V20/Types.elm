module Evergreen.V20.Types exposing (..)

import Browser
import Browser.Dom
import Browser.Navigation
import Lamdera
import Time
import Url


type alias Team = 
    { totalTeamClicks : Int
    , totalTeamPoints : Int
    }


type alias Teams = 
    { realists : Team
    , idealists : Team
    }


type PersonalityType
    = Idealistic
    | Realistic


type alias UserData = 
    { personalityType : PersonalityType
    , sessionId : (Maybe Lamdera.SessionId)
    , username : String
    , userClicks : Int
    , isOnline : Bool
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


type alias FrontendModel =
    { key : Browser.Navigation.Key
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
    }


type alias BackendModel =
    { message : String
    , totalClicks : Int
    , teams : Teams
    , users : (List User)
    , allChatMessages : (List ChatMessage)
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | TryingOutPersonalityType (Maybe PersonalityType)
    | ResetPersonalityType
    | ConfirmedPersonalityType PersonalityType
    | ChangedUsername String
    | FinalizeUser
    | LogUserOut
    | SendClickToBackend
    | SendWantsToSpendToBackend
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
    | OnClientConnect Lamdera.SessionId Lamdera.ClientId
    | OnClientDisconnect Lamdera.SessionId Lamdera.ClientId
    | UpdateTick Time.Posix


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int
    | NewClicksByPersonalityType Teams
    | NewUser User
    | NewTotalUsers Int
    | NewClicksByUser Int
    | NewUsernamesByPersonalityTypes TeamsUserClicks
    | NewTick Time.Posix
    | NewAllChatMessages (List ChatMessage)
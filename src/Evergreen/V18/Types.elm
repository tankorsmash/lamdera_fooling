module Evergreen.V18.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
import Time
import Url


type alias PersonalityTypeDataId = String


type PersonalityType
    = Idealistic
    | Realistic


type alias UserData = 
    { personalityType : PersonalityType
    , sessionId : (Maybe Lamdera.SessionId)
    , username : String
    , userClicks : Int
    }


type User
    = AnonymousUser (Maybe PersonalityType)
    | PreppingUser Lamdera.SessionId PersonalityType
    | FullUser UserData


type alias PersonalityTypeDict a = (Dict.Dict PersonalityTypeDataId a)


type alias ChatMessage = 
    { userData : UserData
    , message : String
    , date : String
    }


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , message : String
    , totalClicksFromBackend : Int
    , personalityTypeClicksFromBackend : (Dict.Dict PersonalityTypeDataId Int)
    , userClicksFromBackend : Int
    , newUsername : String
    , user : User
    , totalUsers : Int
    , usernamesByPersonalityTypes : (PersonalityTypeDict (List (String, Int)))
    , userChatMessage : (Maybe String)
    , allChatMessages : (List ChatMessage)
    }


type alias BackendModel =
    { message : String
    , totalClicks : Int
    , clicksByPersonalityType : (Dict.Dict PersonalityTypeDataId Int)
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
    | UpdateTick Time.Posix


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int
    | NewClicksByPersonalityType (PersonalityTypeDict Int)
    | NewUser User
    | NewTotalUsers Int
    | NewClicksByUser Int
    | NewUsernamesByPersonalityTypes (PersonalityTypeDict (List (String, Int)))
    | NewTick Time.Posix
    | NewAllChatMessages (List ChatMessage)
module Evergreen.V12.Types exposing (..)

import Browser
import Browser.Navigation
import Dict
import Lamdera
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


type alias FrontendModel =
    { key : Browser.Navigation.Key
    , message : String
    , totalClicksFromBackend : Int
    , personalityTypeClicksFromBackend : (Dict.Dict PersonalityTypeDataId Int)
    , userClicksFromBackend : Int
    , username : String
    , user : User
    , totalUsers : Int
    , ignoreme : Int
    }


type alias BackendModel =
    { message : String
    , totalClicks : Int
    , clicksByPersonalityType : (Dict.Dict PersonalityTypeDataId Int)
    , users : (List User)
    }


type FrontendMsg
    = UrlClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | NoOpFrontendMsg
    | SendClickToBackend
    | TryingOutPersonalityType (Maybe PersonalityType)
    | ResetPersonalityType
    | ConfirmedPersonalityType PersonalityType
    | ChangedUsername String
    | FinalizeUser
    | LogUserOut


type ToBackend
    = NoOpToBackend
    | UserGainedAClick
    | UserChoseToBe PersonalityType
    | UserFinalizedUser String
    | UserLoggedOut


type BackendMsg
    = NoOpBackendMsg
    | OnClientConnect Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = NoOpToFrontend
    | NewTotalClicks Int
    | NewClicksByPersonalityType (Dict.Dict PersonalityTypeDataId Int)
    | NewUser User
    | NewTotalUsers Int
    | NewClicksByUser Int

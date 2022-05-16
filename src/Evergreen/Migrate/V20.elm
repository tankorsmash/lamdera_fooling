module Evergreen.Migrate.V20 exposing (..)

import Evergreen.V18.Types as Old
import Evergreen.V20.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    let
        newModel : New.FrontendModel
        newModel =
            { key = old.key
            , message = old.message
            , totalClicksFromBackend = old.totalClicksFromBackend
            , teamsFromBackend =
                { realists = { totalTeamPoints = 0, totalTeamClicks = 0 }
                , idealists = { totalTeamPoints = 0, totalTeamClicks = 0 }
                }
            , userClicksFromBackend = old.userClicksFromBackend
            , newUsername = old.newUsername
            , user = convertUser old.user
            , totalUsers = old.totalUsers
            , teamsUserClicks = { realists = [], idealists = []}
            , allChatMessages = []
            , userChatMessage = Nothing
            }
    in
    ModelMigrated
        ( newModel
        , Cmd.none
        )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated
        ( { message = old.message
          , totalClicks = old.totalClicks
          , teams =
                { realists = { totalTeamPoints = 0, totalTeamClicks = 0 }
                , idealists = { totalTeamPoints = 0, totalTeamClicks = 0 }
                }
          , users = []
          , allChatMessages = []
          }
        , Cmd.none
        )


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgMigrated <|
        case old of
            Old.UrlClicked a ->
                ( New.UrlClicked a, Cmd.none )

            Old.UrlChanged a ->
                ( New.UrlChanged a, Cmd.none )

            Old.NoOpFrontendMsg ->
                ( New.NoOpFrontendMsg, Cmd.none )

            Old.SendClickToBackend ->
                ( New.SendClickToBackend, Cmd.none )

            Old.TryingOutPersonalityType maybePersonalityType ->
                ( New.TryingOutPersonalityType (Maybe.map convertPersonalityType maybePersonalityType), Cmd.none )

            Old.ResetPersonalityType ->
                ( New.ResetPersonalityType, Cmd.none )

            Old.ConfirmedPersonalityType personalityType ->
                ( New.ConfirmedPersonalityType <| convertPersonalityType personalityType, Cmd.none )

            Old.ChangedUsername string ->
                ( New.ChangedUsername string, Cmd.none )

            Old.FinalizeUser ->
                ( New.FinalizeUser, Cmd.none )

            Old.LogUserOut ->
                ( New.LogUserOut, Cmd.none )

            Old.SendWantsToSpendToBackend ->
                ( New.SendWantsToSpendToBackend, Cmd.none )
            Old.ChatInputChanged maybeStr ->
                ( New.ChatInputChanged maybeStr, Cmd.none )
            Old.ChatInputSent ->
                ( New.ChatInputSent , Cmd.none )


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgMigrated <|
        ( case old of
            Old.NoOpBackendMsg ->
                New.NoOpBackendMsg

            Old.OnClientConnect a b ->
                New.OnClientConnect a b

            Old.UpdateTick time ->
                New.UpdateTick time
        , Cmd.none
        )


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgMigrated <|
        ( case old of
            Old.NoOpToFrontend ->
                New.NoOpToFrontend

            Old.NewTotalClicks a ->
                New.NewTotalClicks a

            Old.NewClicksByPersonalityType dict ->
                -- New.NewClicksByPersonalityType dict
                New.NoOpToFrontend

            Old.NewUser user ->
                New.NewUser (convertUser user)

            Old.NewTotalUsers count ->
                New.NewTotalUsers count

            Old.NewClicksByUser count ->
                New.NewClicksByUser count

            Old.NewUsernamesByPersonalityTypes personalityTypeDict ->
                -- New.NewUsernamesByPersonalityTypes personalityTypeDict
                New.NoOpToFrontend

            Old.NewTick time ->
                New.NewTick time

            Old.NewAllChatMessages allChatMessages ->
                New.NewAllChatMessages <| List.map convertChatMessages allChatMessages
        , Cmd.none
        )
        


convertChatMessages : Old.ChatMessage -> New.ChatMessage
convertChatMessages old =
    {userData = convertUserData old.userData, message = old.message, date = old.date}


convertPersonalityType : Old.PersonalityType -> New.PersonalityType
convertPersonalityType old =
    case old of
        Old.Idealistic ->
            New.Idealistic

        Old.Realistic ->
            New.Realistic


convertUser : Old.User -> New.User
convertUser old =
    case old of
        Old.AnonymousUser mbPt ->
            New.AnonymousUser (Maybe.map convertPersonalityType mbPt)

        Old.PreppingUser sessionId pt ->
            New.PreppingUser sessionId (convertPersonalityType pt)

        Old.FullUser userData ->
            New.FullUser (convertUserData userData)


convertUserData : Old.UserData -> New.UserData
convertUserData old =
    { personalityType = convertPersonalityType old.personalityType
    , sessionId = old.sessionId
    , username = old.username
    , userClicks = old.userClicks
    , isOnline = False
    }

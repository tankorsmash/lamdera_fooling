module Evergreen.Migrate.V21 exposing (..)

import Evergreen.V20.Types as Old
import Evergreen.V21.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    let
        newModel : New.FrontendModel
        newModel =
            { key = old.key
            , message = old.message
            , totalClicksFromBackend = old.totalClicksFromBackend
            , teamsFromBackend = old.teamsFromBackend
            , userClicksFromBackend = old.userClicksFromBackend
            , newUsername = old.newUsername
            , user = convertUser old.user
            , totalUsers = old.totalUsers
            , teamsUserClicks = old.teamsUserClicks
            , allChatMessages = []
            , userChatMessage = old.userChatMessage
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
          , teams = convertTeams old.teams
          , users = List.map convertUser old.users
          , allChatMessages = List.map convertChatMessages old.allChatMessages
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
                ( New.ChatInputSent, Cmd.none )

            Old.FocusError htmlId ->
                (New.FocusError htmlId, Cmd.none)



toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgMigrated <|
        case old of
            Old.NoOpToBackend ->
                ( New.NoOpToBackend, Cmd.none )

            Old.UserGainedAClick ->
                ( New.UserGainedAClick, Cmd.none )

            Old.UserChoseToBe personalityType ->
                ( New.UserChoseToBe <| convertPersonalityType personalityType, Cmd.none )

            Old.UserFinalizedUser string ->
                ( New.UserFinalizedUser string, Cmd.none )

            Old.UserLoggedOut ->
                ( New.UserLoggedOut, Cmd.none )

            Old.UserWantsToSpend ->
                (New.UserWantsToSpend, Cmd.none)

            Old.UserSentMessage msg ->
                (New.UserSentMessage msg, Cmd.none)


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


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
    { userData = convertUserData old.userData, message = old.message, date = old.date }


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
    , isOnline = old.isOnline
    , xp = 0
    }

convertTeam : Old.Team -> New.Team
convertTeam old =
    { totalTeamClicks = old.totalTeamClicks
    , totalTeamPoints = old.totalTeamPoints}

convertTeams : Old.Teams -> New.Teams
convertTeams old =
    { realists = convertTeam old.realists
    , idealists = convertTeam old.idealists
    }


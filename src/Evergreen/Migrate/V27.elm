module Evergreen.Migrate.V27 exposing (..)

import Evergreen.V25.Types as Old
import Evergreen.V27.ClickPricing as NewClickPricing
import Evergreen.V27.Types as New
import Lamdera.Migrations exposing (..)
import Time
import UUID


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    let
        newModel : New.FrontendModel
        newModel =
            { key = old.key
            , message = old.message
            , totalClicksFromBackend = old.totalClicksFromBackend
            , teamsFromBackend = convertTeams old.teamsFromBackend
            , userClicksFromBackend = old.userClicksFromBackend
            , newUsername = old.newUsername
            , user = convertUser old.user
            , totalUsers = old.totalUsers
            , teamsUserClicks = old.teamsUserClicks
            , allChatMessages = []
            , userChatMessage = old.userChatMessage
            , --TODO
              lastTick = Time.millisToPosix 0
            , progress = New.Progress 0
            , selfImprovementLevel = NewClickPricing.Level 0
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
                ( New.FocusError htmlId, Cmd.none )

            Old.SendBuyUpgrade upgradeType ->
                ( New.SendBuyUpgrade <| convertUpgradeType upgradeType, Cmd.none )

            Old.TryToJoinGroup groupId ->
                ( New.TryToJoinGroup groupId, Cmd.none )

            Old.TryToLeaveGroup ->
                ( New.TryToLeaveGroup, Cmd.none )


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
                ( New.UserWantsToSpend, Cmd.none )

            Old.UserSentMessage msg ->
                ( New.UserSentMessage msg, Cmd.none )

            Old.UserWantsToBuyUpgrade upgradeType ->
                ( New.UserWantsToBuyUpgrade <| convertUpgradeType upgradeType, Cmd.none )

            Old.UserWantsToJoinGroup groupId ->
                ( New.UserWantsToJoinGroup groupId, Cmd.none )

            Old.UserWantsToLeaveGroup ->
                ( New.UserWantsToLeaveGroup, Cmd.none )


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

            Old.NewTeams dict ->
                New.NewTeams <| convertTeams dict

            Old.NewUser user ->
                New.NewUser (convertUser user)

            Old.NewTotalUsers count ->
                New.NewTotalUsers count

            Old.NewClicksByUser count ->
                New.NewClicksByUser count

            Old.NewUsernamesByPersonalityTypes personalityTypeDict ->
                New.NewUsernamesByPersonalityTypes personalityTypeDict

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
    , xp = old.xp
    , groupId = old.groupId
    , userId = old.userId

    -- TODO
    , selfImprovementLevel = NewClickPricing.Level 0
    }


convertTeam : Old.Team -> New.Team
convertTeam old =
    { totalTeamClicks = old.totalTeamClicks
    , totalTeamPoints = old.totalTeamPoints
    , groups = []
    }


generateUuid : String -> UUID.UUID
generateUuid str =
    UUID.forName str UUID.dnsNamespace


createGroup : String -> New.Group
createGroup groupName =
    { name = groupName, members = [], groupId = generateUuid groupName }


convertTeams : Old.Teams -> New.Teams
convertTeams old =
    let
        newRealistGroups =
            [ createGroup "The Straight Shooters", createGroup "Glasses Half Full" ]

        newRealists =
            convertTeam old.realists
                |> (\realists -> { realists | groups = newRealistGroups })

        newIdealistsGroups =
            [ createGroup "With Eyes Wide Open", createGroup "Excited For The Future" ]

        newIdealists =
            convertTeam old.idealists
                |> (\idealists ->
                        { idealists | groups = newIdealistsGroups }
                   )
    in
    { realists = newRealists
    , idealists = newIdealists
    }


convertUpgradeType : Old.UpgradeType -> New.UpgradeType
convertUpgradeType old =
    case old of
        Old.SelfImprovement level ->
            New.SelfImprovement (NewClickPricing.Level level)

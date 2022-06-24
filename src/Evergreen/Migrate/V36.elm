module Evergreen.Migrate.V36 exposing (..)

import Browser.Navigation exposing (Key)
import Evergreen.V32.ClickPricing as OldClickPricing
import Evergreen.V32.External.Animator.Animator as OldAnimator
import Evergreen.V32.External.Animator.Internal.Time as OldAnimatorTime
import Evergreen.V32.External.Animator.Internal.Timeline as OldTimeline
import Evergreen.V32.Types as Old
import Evergreen.V36.ClickPricing as NewClickPricing
import Evergreen.V36.External.Animator.Animator as NewAnimator
import Evergreen.V36.External.Animator.Internal.Time as NewAnimatorTime
import Evergreen.V36.External.Animator.Internal.Timeline as NewTimeline
import Evergreen.V36.Types as New
import Lamdera.Migrations exposing (..)
import Quantity
import Random
import Time
import UUID
import Url


migrateTeam : Old.Team -> New.Team
migrateTeam old =
    { totalTeamClicks = old.totalTeamClicks
    , totalTeamPoints = old.totalTeamPoints
    , groups = migrateList migrateGroup old.groups
    }


migrateTeams : Old.Teams -> New.Teams
migrateTeams old =
    { realists = migrateTeam old.realists, idealists = migrateTeam old.idealists }


migratePersonalityType : Old.PersonalityType -> New.PersonalityType
migratePersonalityType old =
    case old of
        Old.Idealistic ->
            New.Idealistic

        Old.Realistic ->
            New.Realistic


migrateUserData : Old.UserData -> New.UserData
migrateUserData old =
    { personalityType = migratePersonalityType old.personalityType
    , sessionId = old.sessionId
    , username = old.username
    , userClicks = old.userClicks
    , isOnline = old.isOnline
    , xp = old.xp
    , groupId = migrateMaybe migrateGroupId old.groupId
    , userId = old.userId
    , currentLevels = migrateCurrentLevels old.currentLevels
    }


migrateMaybe =
    Maybe.map


migrateGroupId : Old.GroupId -> New.GroupId
migrateGroupId =
    migrateUUID


migrateUserId =
    migrateUUID


migrateUUID old =
    UUID.toString old
        |> UUID.fromString
        |> Result.withDefault
            (UUID.forName "hope this never happens" UUID.dnsNamespace)



-- migrateUserClickData : Old.UserClickData -> New.UserClickData


migrateUserClickData old =
    old


migrateUser : Old.User -> New.User
migrateUser old =
    case old of
        Old.AnonymousUser a ->
            New.AnonymousUser (Maybe.map migratePersonalityType a)

        Old.PreppingUser a b ->
            New.PreppingUser a (migratePersonalityType b)

        Old.FullUser a ->
            New.FullUser (migrateUserData a)


migrateChatMessageId =
    migrateUUID


migrateList =
    List.map


migrateTeamsUserClicks : Old.TeamsUserClicks -> New.TeamsUserClicks
migrateTeamsUserClicks old =
    { realists = migrateList migrateUserClickData old.realists
    , idealists = migrateList migrateUserClickData old.idealists
    }


migrateChatMessage : Old.ChatMessage -> New.ChatMessage
migrateChatMessage old =
    { userData = migrateUserData old.userData
    , message = old.message
    , date = old.date
    , uuid = migrateChatMessageId old.uuid
    }


myTimeAbsolute : Time.Posix -> NewAnimatorTime.Absolute
myTimeAbsolute posix =
    Quantity.Quantity (toFloat (Time.posixToMillis posix))


myAnimatorInit : a -> NewTimeline.Timeline a
myAnimatorInit first =
    NewTimeline.Timeline
        { initial = first
        , now = myTimeAbsolute (Time.millisToPosix 0)
        , events =
            NewTimeline.Timetable []
        , queued = Nothing
        , interruption = []
        , running = True
        }


migrateCyclingTimeline : Old.CyclingTimeline -> New.CyclingTimeline
migrateCyclingTimeline ( _, value ) =
    ( myAnimatorInit 0, value )


migrateTimelines : Old.Timelines -> New.Timelines
migrateTimelines old =
    { userClicksTimeline = myAnimatorInit <| Just <| New.LabelNumber 0
    , cyclingNumberTimeline = migrateCyclingTimeline old.cyclingNumberTimeline
    }


migrateAdminFrontendModel : Old.AdminFrontendModel -> New.AdminFrontendModel
migrateAdminFrontendModel old =
    { url = old.url
    , key = old.key
    , users = migrateList migrateUser old.users
    , allChatMessages = migrateList migrateChatMessage old.allChatMessages
    , selectedChatMessageUuid = old.selectedChatMessageUuid
    }


migrateFrontendModel : Old.FrontendModel -> New.FrontendModel
migrateFrontendModel old =
    { key = old.key
    , url = old.url
    , device = Debug.todo "Can't handle this"
    , message = old.message
    , totalClicksFromBackend = old.totalClicksFromBackend
    , teamsFromBackend = migrateTeams old.teamsFromBackend
    , userClicksFromBackend = old.userClicksFromBackend
    , newUsername = old.newUsername
    , user = migrateUser old.user
    , totalUsers = old.totalUsers
    , teamsUserClicks = migrateTeamsUserClicks old.teamsUserClicks
    , userChatMessage = old.userChatMessage
    , allChatMessages = List.map migrateChatMessage old.allChatMessages
    , lastTick = old.lastTick
    , timelines = migrateTimelines old.timelines
    , adminFrontendModel = migrateAdminFrontendModel old.adminFrontendModel
    }


migrateAdminSendingToBackend : Old.AdminToBackend -> New.AdminToBackend
migrateAdminSendingToBackend old =
    case old of
        Old.NoOpAdminToBackend ->
            New.NoOpAdminToBackend

        Old.AdminWantsToDownloadUsers ->
            New.AdminWantsToDownloadUsers

        Old.AdminWantsToDownloadChatMessages ->
            New.AdminWantsToDownloadChatMessages

        Old.AdminWantsToAddDummyUsers a ->
            New.AdminWantsToAddDummyUsers a

        Old.AdminWantsToAddDummyChatMessages a ->
            New.AdminWantsToAddDummyChatMessages a

        Old.AdminWantsToDeleteChatMessage a ->
            New.AdminWantsToDeleteChatMessage (migrateChatMessageId a)


migrateLevel : OldClickPricing.Level -> NewClickPricing.Level
migrateLevel old =
    case old of
        OldClickPricing.Level a ->
            NewClickPricing.Level a


migrateCurrentLevel : OldClickPricing.CurrentLevel -> NewClickPricing.CurrentLevel
migrateCurrentLevel old =
    case old of
        OldClickPricing.CurrentLevel level b ->
            NewClickPricing.CurrentLevel
                (migrateLevel level)
                b


migrateCurrentLevels : OldClickPricing.CurrentLevels -> NewClickPricing.CurrentLevels
migrateCurrentLevels old =
    { clickCap = migrateCurrentLevel old.clickCap
    , discuss = migrateCurrentLevel old.discuss
    , argue = migrateCurrentLevel old.argue
    , energize = migrateCurrentLevel old.energize
    , energizeCycleCap = migrateCurrentLevel old.energizeCycleCap
    }


migrateGroup : Old.Group -> New.Group
migrateGroup old =
    { members = List.map migrateUserId old.members, name = old.name, groupId = old.groupId }


migrateUpgradeType : Old.UpgradeType -> New.UpgradeType
migrateUpgradeType old =
    case old of
        Old.Discussion a ->
            New.Discussion (migrateLevel a)

        Old.Argumentation a ->
            New.Argumentation (migrateLevel a)

        Old.Energization a ->
            New.Energization (migrateLevel a)

        Old.EnergizeCap a ->
            New.EnergizeCap (migrateLevel a)

        Old.ClickCap a ->
            New.ClickCap (migrateLevel a)


migrateToBackend : Old.ToBackend -> New.ToBackend
migrateToBackend old =
    case old of
        Old.NoOpToBackend ->
            New.NoOpToBackend

        Old.UserGainedAClick ->
            New.UserGainedAClick

        Old.UserDiscussed ->
            New.UserDiscussed

        Old.UserArgued ->
            New.UserArgued

        Old.UserEnergized ->
            New.UserEnergized

        Old.UserWantsToSpend ->
            New.UserWantsToSpend

        Old.UserChoseToBe a ->
            New.UserChoseToBe (migratePersonalityType a)

        Old.UserFinalizedUser a ->
            New.UserFinalizedUser a

        Old.UserLoggedOut ->
            New.UserLoggedOut

        Old.UserSentMessage a ->
            New.UserSentMessage a

        Old.UserWantsToBuyUpgrade a ->
            New.UserWantsToBuyUpgrade (migrateUpgradeType a)

        Old.UserWantsToJoinGroup a ->
            New.UserWantsToJoinGroup (migrateUUID a)

        Old.UserWantsToLeaveGroup ->
            New.UserWantsToLeaveGroup

        Old.AdminSendingToBackend adminToBackend ->
            Debug.todo "branch 'AdminSendingToBackend _' not implemented"


migrateFrontendMsg : Old.FrontendMsg -> New.FrontendMsg
migrateFrontendMsg old =
    case old of
        Old.UrlClicked url ->
            New.UrlClicked url

        Old.UrlChanged url ->
            New.UrlChanged url

        Old.NoOpFrontendMsg ->
            New.NoOpFrontendMsg

        Old.LocalTick time ->
            New.LocalTick time

        Old.TryingOutPersonalityType personalityType ->
            New.TryingOutPersonalityType (Maybe.map migratePersonalityType personalityType)

        Old.ResetPersonalityType ->
            New.ResetPersonalityType

        Old.ConfirmedPersonalityType personalityType ->
            New.ConfirmedPersonalityType (migratePersonalityType personalityType)

        Old.ChangedUsername username ->
            New.ChangedUsername username

        Old.FinalizeUser ->
            New.FinalizeUser

        Old.LogUserOut ->
            New.LogUserOut

        Old.SendClickToBackend ->
            New.SendClickToBackend

        Old.Discuss ->
            New.Discuss

        Old.Argue ->
            New.Argue

        Old.CollectEnergize ->
            New.CollectEnergize

        Old.SendWantsToSpendToBackend ->
            New.SendWantsToSpendToBackend

        Old.SendBuyUpgrade a ->
            New.SendBuyUpgrade (migrateUpgradeType a)

        Old.TryToJoinGroup a ->
            New.TryToJoinGroup (migrateUUID a)

        Old.TryToLeaveGroup ->
            New.TryToLeaveGroup

        Old.ChatInputChanged chatMessage ->
            New.ChatInputChanged chatMessage

        Old.ChatInputSent ->
            New.ChatInputSent

        Old.FocusError a ->
            New.FocusError a

        Old.GotAdminFrontendMsg adminFrontendMsg ->
            New.GotAdminFrontendMsg (migrateAdminFrontendMsg adminFrontendMsg)


migrateAdminFrontendMsg : Old.AdminFrontendMsg -> New.AdminFrontendMsg
migrateAdminFrontendMsg old =
    case old of
        Old.NoOpAdminFrontend ->
            New.NoOpAdminFrontend

        Old.DownloadUsers ->
            New.DownloadUsers

        Old.DownloadAllChatMessages ->
            New.DownloadAllChatMessages

        Old.AddDummyUsers a ->
            New.AddDummyUsers a

        Old.AddDummyChatMessages a ->
            New.AddDummyChatMessages a

        Old.SelectChatMessage a ->
            New.SelectChatMessage (migrateMaybe migrateChatMessageId a)

        Old.DeleteSelectedMessage a ->
            New.DeleteSelectedMessage (migrateChatMessageId a)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated ( migrateFrontendModel old, Cmd.none )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgMigrated <| ( migrateFrontendMsg old, Cmd.none )


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgMigrated <| ( migrateToBackend old, Cmd.none )


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged

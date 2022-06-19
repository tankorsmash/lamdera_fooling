module Evergreen.Migrate.V32 exposing (..)

import Evergreen.V30.ClickPricing as OldClickPricing
import Evergreen.V30.External.Animator.Animator as OldAnimator
import Evergreen.V30.External.Animator.Internal.Time as OldAnimatorTime
import Evergreen.V30.External.Animator.Internal.Timeline as OldTimeline
import Evergreen.V30.Types as Old
import Evergreen.V32.ClickPricing as NewClickPricing
import Evergreen.V32.External.Animator.Animator as NewAnimator
import Evergreen.V32.External.Animator.Internal.Time as NewAnimatorTime
import Evergreen.V32.External.Animator.Internal.Timeline as NewTimeline
import Evergreen.V32.Types as New
import Lamdera.Migrations exposing (..)
import Quantity
import Random
import Time
import UUID
import Url
import Browser.Navigation exposing (Key)


migratePersonalityType : Old.PersonalityType -> New.PersonalityType
migratePersonalityType old =
    case old of
        Old.Idealistic ->
            New.Idealistic

        Old.Realistic ->
            New.Realistic


migrateGroupId : Old.GroupId -> New.GroupId
migrateGroupId old =
    UUID.toString old
        |> UUID.fromString
        |> Result.withDefault
            (UUID.forName "hope this never happens" UUID.dnsNamespace)


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



-- let
--     newCurrentLevels : NewClickPricing.CurrentLevels
--     newCurrentLevels =
--         NewClickPricing.CurrentLevels


migrateUserData : Old.UserData -> New.UserData
migrateUserData old =
    { personalityType = migratePersonalityType old.personalityType
    , sessionId = old.sessionId
    , username = old.username
    , userClicks = old.userClicks
    , isOnline = old.isOnline
    , xp = old.xp
    , groupId = Maybe.map migrateGroupId old.groupId
    , userId = old.userId
    , currentLevels = migrateCurrentLevels old.currentLevels
    }


migrateUser : Old.User -> New.User
migrateUser old =
    case old of
        Old.AnonymousUser a ->
            New.AnonymousUser (Maybe.map migratePersonalityType a)

        Old.PreppingUser sessionId b ->
            New.PreppingUser sessionId (migratePersonalityType b)

        Old.FullUser a ->
            New.FullUser (migrateUserData a)


migrateUUID old =
    UUID.toString old
        |> UUID.fromString
        |> Result.withDefault
            (UUID.forName "hope this never happens" UUID.dnsNamespace)



-- migrateUserId : Old.UserId -> New.UserId


migrateUserId old =
    UUID.toString old
        |> UUID.fromString
        |> Result.withDefault
            (UUID.forName "hope this never happens" UUID.dnsNamespace)


migrateGroup : Old.Group -> New.Group
migrateGroup old =
    { members = List.map migrateUserId old.members, name = old.name, groupId = old.groupId }


migrateTeam : Old.Team -> New.Team
migrateTeam old =
    { totalTeamClicks = old.totalTeamClicks
    , totalTeamPoints = old.totalTeamPoints
    , groups = List.map migrateGroup old.groups
    }


migrateTeams : Old.Teams -> New.Teams
migrateTeams old =
    { realists = migrateTeam old.realists, idealists = migrateTeam old.idealists }



-- migrateUserClickData : Old.UserClickData -> New.UserClickData


migrateUserClickData old =
    old


migrateTeamsUserClicks : Old.TeamsUserClicks -> New.TeamsUserClicks
migrateTeamsUserClicks old =
    { realists = List.map migrateUserClickData old.realists
    , idealists = List.map migrateUserClickData old.idealists
    }


migrateChatMessage : Old.ChatMessage -> New.ChatMessage
migrateChatMessage old =
    let
        uuid : UUID.UUID
        uuid =
            buildChatMessageUuuid old.userData.username old.message newDate

        newDate = Time.millisToPosix 0
    in
    { userData = migrateUserData old.userData
    , message = old.message
    , date = newDate
    , uuid = uuid
    }


buildChatMessageUuuid : String -> String -> Time.Posix -> UUID.UUID
buildChatMessageUuuid username message timestamp =
    timestamp
        |> Time.posixToMillis
        |> String.fromInt
        |> (++) (message ++ username)
        |> generateUuid


generateUuid : String -> UUID.UUID
generateUuid str =
    UUID.forName str UUID.dnsNamespace


migrateFrontendModel : Old.FrontendModel -> New.FrontendModel
migrateFrontendModel old =
    { key = old.key
    , url = old.url
    , message = old.message
    , totalClicksFromBackend = old.totalClicksFromBackend
    , teamsFromBackend = migrateTeams old.teamsFromBackend
    , userClicksFromBackend = old.userClicksFromBackend
    , newUsername = old.newUsername
    , user = migrateUser old.user
    , totalUsers = old.totalUsers
    , teamsUserClicks = migrateTeamsUserClicks old.teamsUserClicks
    , userChatMessage = old.userChatMessage
    , allChatMessages = migrateList migrateChatMessage old.allChatMessages
    , lastTick = old.lastTick
    , timelines = migrateTimelines old.timelines
    , --TODO use the v32+ one
    adminFrontendModel = initAdminFrontendModel old.url old.key
    }

initAdminFrontendModel : Url.Url -> Key -> New.AdminFrontendModel
initAdminFrontendModel url key =
    { url = url
    , key = key
    , users = []
    , allChatMessages = []
    , selectedChatMessageUuid = Nothing
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



-- migrateTimeline : OldTimeline.Timeline a -> NewTimeline.Timeline b
-- migrateTimeline old =
--     case old of
--         OldTimeline.Timeline a ->
--             myAnimatorInit a


migrateCyclingTimeline : Old.CyclingTimeline -> New.CyclingTimeline
migrateCyclingTimeline ( _, value ) =
    ( myAnimatorInit 0, value )


migrateTimelines : Old.Timelines -> New.Timelines
migrateTimelines old =
    { userClicksTimeline = myAnimatorInit <| Just <| New.LabelNumber 0
    , cyclingNumberTimeline = migrateCyclingTimeline old.cyclingNumberTimeline
    }


migrateList =
    List.map


migrateBackendModel : Old.BackendModel -> New.BackendModel
migrateBackendModel old =
    { message = old.message
    , totalClicks = old.totalClicks
    , teams = migrateTeams old.teams
    , users = migrateList migrateUser old.users
    , allChatMessages = migrateList migrateChatMessage old.allChatMessages
    , lastTick = old.lastTick
    , --TODO replace this with next versions' new
      globalSeed = Random.initialSeed 123
    }


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


migrateMaybe =
    Maybe.map


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


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated <| ( migrateFrontendModel old, Cmd.none )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated <| ( migrateBackendModel old, Cmd.none )


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
    MsgMigrated <| ( migrateToFrontend old, Cmd.none )


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


migrateToFrontend : Old.ToFrontend -> New.ToFrontend
migrateToFrontend old =
    case old of
        Old.NoOpToFrontend ->
            New.NoOpToFrontend

        Old.NewTotalClicks a ->
            New.NewTotalClicks a

        Old.NewTeams teams ->
            New.NewTeams (migrateTeams teams)

        Old.NewUser userData ->
            New.NewUser (migrateUser userData)

        Old.NewTotalUsers users ->
            New.NewTotalUsers users

        Old.NewClicksByUser clicks ->
            New.NewClicksByUser clicks

        Old.NewUsernamesByPersonalityTypes a ->
            New.NewUsernamesByPersonalityTypes (migrateTeamsUserClicks a)

        Old.NewTick a ->
            New.NewTick (migratePosix a)

        Old.NewAllChatMessages a ->
            New.NewAllChatMessages (migrateList migrateChatMessage a)


migratePosix =
    identity

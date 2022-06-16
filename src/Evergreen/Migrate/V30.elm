module Evergreen.Migrate.V30 exposing (..)

import Evergreen.V28.ClickPricing as OldClickPricing
import Evergreen.V28.External.Animator.Animator as OldAnimator
import Evergreen.V28.External.Animator.Internal.Time as OldAnimatorTime
import Evergreen.V28.External.Animator.Internal.Timeline as OldTimeline
import Evergreen.V28.Types as Old
import Evergreen.V30.ClickPricing as NewClickPricing
import Evergreen.V30.External.Animator.Animator as NewAnimator
import Evergreen.V30.External.Animator.Internal.Time as NewAnimatorTime
import Evergreen.V30.External.Animator.Internal.Timeline as NewTimeline
import Evergreen.V30.Types as New
import Lamdera.Migrations exposing (..)
import Quantity
import Time
import UUID
import Url
import Url.Builder


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
    , groupId = Maybe.map (migrateGroupId) old.groupId
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



-- migrateUserId : Old.UserId -> New.UserId
migrateUserId old =
    UUID.toString old
        |> UUID.fromString
        |> Result.withDefault
            (UUID.forName "hope this never happens" UUID.dnsNamespace)

-- convertUser : Old.User -> New.User
-- convertUser old =
--     case old of
--         Old.AnonymousUser mbPt ->
--             New.AnonymousUser (Maybe.map convertPersonalityType mbPt)
--
--         Old.PreppingUser sessionId pt ->
--             New.PreppingUser sessionId (convertPersonalityType pt)
--
--         Old.FullUser userData ->
--             New.FullUser (convertUserData userData)



-- convertUserData : Old.UserData -> New.UserData
-- convertUserData old =
--     { personalityType = convertPersonalityType old.personalityType
--     , sessionId = old.sessionId
--     , username = old.username
--     , userClicks = old.userClicks
--     , isOnline = old.isOnline
--     , xp = old.xp
--     , groupId = old.groupId
--     , userId = old.userId
--
--     -- TODO
--     , currentLevels =
--         { clickCap = NewClickPricing.CurrentLevel (NewClickPricing.Level 0) Nothing
--         , discuss = NewClickPricing.CurrentLevel (NewClickPricing.Level 0) Nothing
--         , argue = NewClickPricing.CurrentLevel (NewClickPricing.Level 0) Nothing
--         , energize = NewClickPricing.CurrentLevel (NewClickPricing.Level 0) Nothing
--         , energizeCycleCap = NewClickPricing.CurrentLevel (NewClickPricing.Level 0) Nothing
--         }
--     }
--


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
    { userData = migrateUserData old.userData, message = old.message, date = old.date }


migrateFrontendModel : Old.FrontendModel -> New.FrontendModel
migrateFrontendModel old =
    { key = old.key

    -- TODO use the old url in v30+
    , url = { protocol = Url.Http
        , host = ""
        , port_ = Nothing
        , path = "/"
        , query = Nothing
        , fragment = Nothing
        }
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


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated (migrateFrontendModel old, Cmd.none)


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


frontendMsg : Old.FrontendMsg -> MsgMigration New.FrontendMsg New.FrontendMsg
frontendMsg old =
    MsgUnchanged


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgUnchanged


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgUnchanged

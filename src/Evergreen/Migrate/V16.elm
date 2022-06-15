module Evergreen.Migrate.V16 exposing (..)

import Evergreen.V15.Types as Old
import Evergreen.V16.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


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
    MsgMigrated <|
        ( case old of
            Old.NoOpBackendMsg ->
                New.NoOpBackendMsg

            Old.OnClientConnect a b ->
                New.OnClientConnect a b
        , Cmd.none
        )


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgMigrated <|
        ( case old of
            Old.NoOpToFrontend ->
                New.NoOpToFrontend

            Old.NewTotalClicks int ->
                New.NewTotalClicks int

            Old.NewClicksByPersonalityType persDict ->
                New.NewClicksByPersonalityType persDict

            Old.NewUser user ->
                New.NewUser <| convertUser user

            Old.NewTotalUsers int ->
                New.NewTotalUsers int

            Old.NewClicksByUser int ->
                New.NewClicksByUser int

            Old.NewUsernamesByPersonalityTypes specialPersDict ->
                New.NewUsernamesByPersonalityTypes specialPersDict
        , Cmd.none
        )


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


convertUserData old =
    { personalityType = convertPersonalityType old.personalityType
    , sessionId = old.sessionId
    , username = old.username
    , userClicks = old.userClicks
    }

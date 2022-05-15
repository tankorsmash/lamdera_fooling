module Evergreen.Migrate.V18 exposing (..)

import Evergreen.V16.Types as Old
import Evergreen.V18.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelUnchanged


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelUnchanged


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
                New.NewClicksByPersonalityType dict

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

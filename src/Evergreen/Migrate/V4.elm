module Evergreen.Migrate.V4 exposing (..)

import Evergreen.V1.Types as Old
import Evergreen.V4.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    let
        new =
            { message = old.message, clicksFromBackend = 0, key = old.key }
    in
    ModelMigrated
        ( new, Cmd.none )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    let
        new =
            { message = old.message, clicks = 0 }
    in
    ModelMigrated
        ( new, Cmd.none )


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


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgMigrated <|
        case old of
            Old.NoOpToBackend ->
                ( New.NoOpToBackend, Cmd.none )


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgMigrated <|
        case old of
            Old.NoOpToFrontend ->
                ( New.NoOpToFrontend, Cmd.none )

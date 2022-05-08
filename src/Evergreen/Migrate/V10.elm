module Evergreen.Migrate.V10 exposing (..)

import Dict
import Evergreen.V10.Types as New
import Evergreen.V6.Types as Old
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated
        ( { key = old.key
          , message = old.message
          , totalClicksFromBackend = old.clicksFromBackend
          , personalityTypeClicksFromBackend = Dict.empty
          , userClicksFromBackend = 0
          , username = ""
          , user = New.AnonymousUser Nothing
          , totalUsers = 0
          }
        , Cmd.none
        )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated
        ( { message = old.message
          , totalClicks = old.clicks
          , clicksByPersonalityType = Dict.empty
          , users = []
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


toBackend : Old.ToBackend -> MsgMigration New.ToBackend New.BackendMsg
toBackend old =
    MsgMigrated <|
        case old of
            Old.NoOpToBackend ->
                ( New.NoOpToBackend, Cmd.none )

            Old.ToBackendClick ->
                ( New.UserGainedAClick, Cmd.none )


backendMsg : Old.BackendMsg -> MsgMigration New.BackendMsg New.BackendMsg
backendMsg old =
    MsgUnchanged


toFrontend : Old.ToFrontend -> MsgMigration New.ToFrontend New.FrontendMsg
toFrontend old =
    MsgMigrated <|
        case old of
            Old.NoOpToFrontend ->
                ( New.NoOpToFrontend, Cmd.none )

            Old.NewTotalClicks a ->
                ( New.NewTotalClicks a, Cmd.none )

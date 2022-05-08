module Evergreen.Migrate.V12 exposing (..)

import Dict
import Evergreen.V10.Types as Old
import Evergreen.V12.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    ModelMigrated
        ( { key = old.key
          , message = old.message
          , totalClicksFromBackend = old.totalClicksFromBackend
          , personalityTypeClicksFromBackend = Dict.empty
          , userClicksFromBackend = 0
          , username = ""
          , user = New.AnonymousUser Nothing
          , totalUsers = 0
          , ignoreme = -1
          }
        , Cmd.none
        )


backendModel : Old.BackendModel -> ModelMigration New.BackendModel New.BackendMsg
backendModel old =
    ModelMigrated
        ( { message = old.message
          , totalClicks = old.totalClicks
          , clicksByPersonalityType =
                Dict.fromList
                    [ ( "Idealistic", 0 )
                    , ( "Realistic", 0 )
                    ]
          , users = []
          }
        , Cmd.none
        )


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

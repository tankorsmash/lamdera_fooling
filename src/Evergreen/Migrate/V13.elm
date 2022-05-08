module Evergreen.Migrate.V13 exposing (..)

import Dict
import Evergreen.V12.Types as Old
import Evergreen.V13.Types as New
import Lamdera.Migrations exposing (..)


frontendModel : Old.FrontendModel -> ModelMigration New.FrontendModel New.FrontendMsg
frontendModel old =
    let
        newModel : New.FrontendModel
        newModel =
            { key = old.key
            , message = old.message
            , totalClicksFromBackend = old.totalClicksFromBackend
            , personalityTypeClicksFromBackend = old.personalityTypeClicksFromBackend
            , userClicksFromBackend = old.userClicksFromBackend
            , username = old.username
            , user = convertUser old.user
            , totalUsers = old.totalUsers
            , ignoreme = old.ignoreme
            , usernamesByPersonalityTypes = Dict.empty
            }
    in
    ModelMigrated
        ( newModel
        , Cmd.none
        )


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
    MsgMigrated <|
        (case old of
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
                , Cmd.none)


convertPersonalityType : Old.PersonalityType -> New.PersonalityType
convertPersonalityType old =
    case old of
        Old.Idealistic -> New.Idealistic
        Old.Realistic -> New.Realistic

convertUser : Old.User -> New.User
convertUser old =
    case old of
        Old.AnonymousUser mbPt ->
            New.AnonymousUser (Maybe.map (convertPersonalityType ) mbPt)

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

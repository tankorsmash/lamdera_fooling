module Backend exposing (..)

import Dict
import Lamdera exposing (SessionId)
import List.Extra
import Process
import Task
import Time
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = subscriptions
        }


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect OnClientConnect
        ]


init : ( Model, Cmd BackendMsg )
init =
    ( initBackendModel
    , Task.perform UpdateTick Time.now
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        OnClientConnect sessionId clientId ->
            ( model
            , Cmd.batch
                [ Lamdera.sendToFrontend clientId (NewTotalClicks model.totalClicks)
                , Lamdera.sendToFrontend clientId (NewTotalUsers <| List.length model.users)
                , case getUserBySessionId model.users sessionId of
                    Just user ->
                        Lamdera.sendToFrontend sessionId (NewUser user)

                    Nothing ->
                        Cmd.none

                -- , Lamdera.sendToFrontend clientId
                , Lamdera.sendToFrontend clientId (NewUsernamesByPersonalityTypes (usernamesDataByPersonalityTypes model.users))
                , Lamdera.sendToFrontend clientId (NewClicksByPersonalityType model.clicksByPersonalityType)
                ]
            )

        UpdateTick time ->
            ( model
            , Cmd.batch
                [ Process.sleep 1000
                    -- |> Task.andThen (\_ -> Task.succeed UpdateTick )
                    -- |> Task.perform Time.now
                    |> Task.andThen (\_ -> Time.now)
                    |> Task.perform UpdateTick
                , Lamdera.broadcast (NewTotalClicks model.totalClicks)
                , Lamdera.broadcast (NewTick time)
                ]
            )


usernamesDataByPersonalityTypes : List User -> PersonalityTypeDict (List ( String, Int ))
usernamesDataByPersonalityTypes users =
    List.foldl
        (\user acc ->
            case user of
                AnonymousUser _ ->
                    acc

                PreppingUser _ _ ->
                    acc

                FullUser userData ->
                    Dict.update
                        (personalityTypeToDataId userData.personalityType)
                        (\v ->
                            let
                                toAdd =
                                    ( userData.username, userData.userClicks )
                            in
                            case v of
                                Just names ->
                                    Just (toAdd :: names)

                                Nothing ->
                                    Just [ toAdd ]
                        )
                        acc
        )
        Dict.empty
        users


updateFromFrontend : SessionId -> SessionId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        UserGainedAClick ->
            getUserBySessionId model.users sessionId
                |> Maybe.andThen getUserData
                |> Maybe.map
                    (\userData ->
                        let
                            modifyClicks clicks =
                                clicks + 1

                            updatedClicksTracker =
                                Dict.update
                                    (personalityTypeToDataId userData.personalityType)
                                    (Maybe.map modifyClicks)
                                    model.clicksByPersonalityType

                            newUsers =
                                model.users
                                    |> List.Extra.updateIf
                                        (\u ->
                                            getUsername u
                                                |> Maybe.map ((==) userData.username)
                                                |> Maybe.withDefault False
                                        )
                                        (\oldUser ->
                                            mapUserData oldUser
                                                (\ud ->
                                                    { ud | userClicks = modifyClicks ud.userClicks }
                                                )
                                                |> Maybe.map FullUser
                                                |> Maybe.withDefault oldUser
                                        )

                            newModel : Model
                            newModel =
                                { model
                                    | totalClicks = modifyClicks model.totalClicks
                                    , clicksByPersonalityType = updatedClicksTracker
                                    , users = newUsers
                                }
                        in
                        ( newModel
                        , Cmd.batch
                            [ Lamdera.broadcast (NewTotalClicks newModel.totalClicks)
                            , Lamdera.broadcast (NewClicksByPersonalityType newModel.clicksByPersonalityType)
                            , Lamdera.broadcast (NewUsernamesByPersonalityTypes (usernamesDataByPersonalityTypes newModel.users))
                            , Lamdera.sendToFrontend clientId (NewClicksByUser <| modifyClicks userData.userClicks)
                            ]
                        )
                    )
                |> Maybe.withDefault noop

        UserWantsToSpend ->
            getUserBySessionId model.users sessionId
                |> Maybe.andThen getUserData
                |> Maybe.map
                    (\userData ->
                        let
                            modifyClicks clicks =
                                clicks - 1

                            updatedClicksTracker =
                                Dict.update
                                    (personalityTypeToDataId userData.personalityType)
                                    (Maybe.map modifyClicks)
                                    model.clicksByPersonalityType

                            newUsers =
                                model.users
                                    |> List.Extra.updateIf
                                        (\u ->
                                            getUsername u
                                                |> Maybe.map ((==) userData.username)
                                                |> Maybe.withDefault False
                                        )
                                        (\oldUser ->
                                            mapUserData oldUser
                                                (\ud ->
                                                    { ud | userClicks = modifyClicks ud.userClicks }
                                                )
                                                |> Maybe.map FullUser
                                                |> Maybe.withDefault oldUser
                                        )

                            newModel : Model
                            newModel =
                                { model
                                    | totalClicks = modifyClicks model.totalClicks
                                    , clicksByPersonalityType = updatedClicksTracker
                                    , users = newUsers
                                }
                        in
                        ( newModel
                        , Cmd.batch
                            [ Lamdera.broadcast (NewTotalClicks newModel.totalClicks)
                            , Lamdera.broadcast (NewClicksByPersonalityType newModel.clicksByPersonalityType)
                            , Lamdera.broadcast (NewUsernamesByPersonalityTypes (usernamesDataByPersonalityTypes newModel.users))
                            , Lamdera.sendToFrontend clientId (NewClicksByUser <| modifyClicks userData.userClicks)
                            ]
                        )
                    )
                |> Maybe.withDefault noop

        UserChoseToBe personalityType ->
            let
                existingUser =
                    model.users
                        |> List.filter
                            (\user ->
                                getSessionId user
                                    |> Maybe.map (\cid -> cid == sessionId)
                                    |> Maybe.withDefault False
                            )
                        |> List.head

                toCreate : User
                toCreate =
                    PreppingUser sessionId personalityType

                newModel =
                    case existingUser of
                        -- if user exists, replace it
                        Just user ->
                            { model
                                | users =
                                    List.Extra.updateIf
                                        (\u ->
                                            getSessionId u
                                                |> Maybe.map (\cid -> cid == sessionId)
                                                |> Maybe.withDefault False
                                        )
                                        (always toCreate)
                                        model.users
                            }

                        -- otherwise add it
                        Nothing ->
                            { model | users = toCreate :: model.users }
            in
            ( newModel
            , Cmd.batch
                [ Lamdera.sendToFrontend sessionId (NewUser toCreate)
                , Lamdera.broadcast (NewUsernamesByPersonalityTypes (usernamesDataByPersonalityTypes model.users))
                ]
            )

        UserFinalizedUser username ->
            let
                existingUser =
                    getUserBySessionId model.users sessionId

                newModel =
                    case existingUser of
                        -- if user exists, replace it
                        Just user ->
                            { model
                                | users =
                                    List.Extra.updateIf
                                        (\u ->
                                            Types.getSessionId u
                                                |> Maybe.map ((==) sessionId)
                                                |> Maybe.withDefault False
                                        )
                                        (\u ->
                                            case u of
                                                AnonymousUser _ ->
                                                    u

                                                PreppingUser sessionId_ personalityType ->
                                                    FullUser
                                                        { sessionId = Just sessionId_
                                                        , username = username
                                                        , personalityType = personalityType
                                                        , userClicks = 0
                                                        }

                                                FullUser _ ->
                                                    u
                                        )
                                        model.users
                            }

                        -- otherwise do nothing
                        Nothing ->
                            model
            in
            ( newModel
            , getUserByUsername newModel.users username
                |> Maybe.map
                    (\u ->
                        Cmd.batch
                            [ Lamdera.sendToFrontend sessionId <| NewUser u
                            , Lamdera.broadcast (NewUsernamesByPersonalityTypes (usernamesDataByPersonalityTypes newModel.users))
                            ]
                    )
                |> Maybe.withDefault Cmd.none
            )

        UserLoggedOut ->
            let
                existingUser =
                    getUserBySessionId model.users sessionId

                toCreate =
                    AnonymousUser Nothing

                newModel =
                    { model
                        | users =
                            List.Extra.updateIf
                                (\u ->
                                    getSessionId u
                                        |> Maybe.map (\cid -> cid == sessionId)
                                        |> Maybe.withDefault False
                                )
                                (\u ->
                                    case u of
                                        AnonymousUser _ ->
                                            u

                                        PreppingUser sid pt ->
                                            toCreate

                                        FullUser userData ->
                                            FullUser { userData | sessionId = Nothing }
                                )
                                model.users
                    }
            in
            ( newModel
            , Lamdera.sendToFrontend sessionId <| NewUser toCreate
            )


{-| Basically setTimeout that'll make a Msg come through `millis` milliseconds
later
-}
delay : Float -> msg -> Cmd msg
delay millis msg =
    Process.sleep millis
        |> Task.perform (\_ -> msg)


getUserBySessionId : List User -> SessionId -> Maybe User
getUserBySessionId users sessionId =
    users
        |> List.filter
            (getSessionId
                >> Maybe.map ((==) sessionId)
                >> Maybe.withDefault False
            )
        |> List.head


getUserByUsername : List User -> String -> Maybe User
getUserByUsername users username =
    users
        |> List.filter
            (\u ->
                mapUserData u .username
                    |> Maybe.map ((==) username)
                    |> Maybe.withDefault False
            )
        |> List.head

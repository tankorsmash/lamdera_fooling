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
                , Lamdera.sendToFrontend clientId (NewAllChatMessages model.allChatMessages)
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


userMatchesSessionId : SessionId -> User -> Bool
userMatchesSessionId sessionId user =
    Types.getSessionId user
        |> Maybe.map ((==) sessionId)
        |> Maybe.withDefault False


userMatchesUsername : String -> User -> Bool
userMatchesUsername username user =
    Types.getUsername user
        |> Maybe.map ((==) username)
        |> Maybe.withDefault False


userIsPrepping : User -> Bool
userIsPrepping user =
    case user of
        AnonymousUser _ ->
            False

        PreppingUser _ _ ->
            True

        FullUser _ ->
            False


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
                |> Debug.log "clicking user?"
                |> Maybe.andThen getUserData
                |> Debug.log "clicking userdata?"
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
            let
                clickCost =
                    3
            in
            getUserBySessionId model.users sessionId
                |> Maybe.andThen getUserData
                |> --kinda hackily make sure they can afford this by returning nothing. not sure if this is elm style
                   Maybe.andThen
                    (\userData ->
                        if userData.userClicks < clickCost then
                            Nothing

                        else
                            Just userData
                    )
                |> Maybe.map
                    (\userData ->
                        let
                            modifySelfClicks clicks =
                                clicks - clickCost

                            modifyEnemyClicks clicks =
                                clicks - 1

                            updatedClicksTracker =
                                model.clicksByPersonalityType
                                    |> --remove their own clicks
                                       Dict.update
                                        (personalityTypeToDataId userData.personalityType)
                                        (Maybe.map modifySelfClicks)
                                    |> --remove enemy teams clicks
                                       Dict.update
                                        (case userData.personalityType of
                                            Realistic ->
                                                personalityTypeToDataId Idealistic

                                            Idealistic ->
                                                personalityTypeToDataId Realistic
                                        )
                                        (Maybe.map modifyEnemyClicks)

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
                                                    { ud | userClicks = modifySelfClicks ud.userClicks }
                                                )
                                                |> Maybe.map FullUser
                                                |> Maybe.withDefault oldUser
                                        )

                            newModel : Model
                            newModel =
                                { model
                                    | totalClicks = modifySelfClicks model.totalClicks
                                    , clicksByPersonalityType = updatedClicksTracker
                                    , users = newUsers
                                }
                        in
                        ( newModel
                        , Cmd.batch
                            [ Lamdera.broadcast (NewTotalClicks newModel.totalClicks)
                            , Lamdera.broadcast (NewClicksByPersonalityType newModel.clicksByPersonalityType)
                            , Lamdera.broadcast (NewUsernamesByPersonalityTypes (usernamesDataByPersonalityTypes newModel.users))
                            , Lamdera.sendToFrontend clientId (NewClicksByUser <| modifySelfClicks userData.userClicks)
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
                existingUserBySession =
                    getUserBySessionId model.users sessionId

                existingUserByUsername =
                    getUserByUsername model.users username

                newModel =
                    case Debug.log "matching users" ( existingUserBySession, existingUserByUsername ) of
                        -- if user with session id exists, replace it with a reset one
                        ( Just sessionUser, Nothing ) ->
                            let
                                _ =
                                    Debug.log "sessionid matches" 123
                            in
                            { model
                                | users =
                                    List.Extra.updateIf
                                        (userMatchesSessionId sessionId)
                                        (\u ->
                                            case u of
                                                AnonymousUser _ ->
                                                    u

                                                PreppingUser sessionId_ personalityType ->
                                                    --promote to full user
                                                    FullUser
                                                        { sessionId = Just sessionId_
                                                        , username = username
                                                        , personalityType = personalityType
                                                        , userClicks = 0
                                                        }

                                                FullUser userData ->
                                                    -- just replace session id, dont reset
                                                    FullUser
                                                        { sessionId = Just sessionId
                                                        , username = username
                                                        , personalityType = userData.personalityType
                                                        , userClicks = userData.userClicks
                                                        }
                                        )
                                        model.users
                            }

                        -- if the username exists on a user, just take it over
                        ( _, Just usernameUser ) ->
                            model
                                |> (\m ->
                                        { m
                                            | users =
                                                List.Extra.updateIf
                                                    (userMatchesUsername username)
                                                    (\u ->
                                                        case u of
                                                            AnonymousUser _ ->
                                                                u

                                                            PreppingUser _ personalityType ->
                                                                --promote to full user
                                                                FullUser
                                                                    { sessionId = Just sessionId
                                                                    , username = username
                                                                    , personalityType = personalityType
                                                                    , userClicks = 0
                                                                    }

                                                            FullUser userData ->
                                                                -- override session id
                                                                FullUser
                                                                    { sessionId = Just sessionId
                                                                    , username = username
                                                                    , personalityType = userData.personalityType
                                                                    , userClicks = userData.userClicks
                                                                    }
                                                    )
                                                    model.users
                                        }
                                   )
                                |> --remove old prepping user now that its been promoted
                                   (\m ->
                                        let
                                            newUsers =
                                                List.partition
                                                    (\u ->
                                                        userMatchesSessionId sessionId u
                                                            && userIsPrepping u
                                                    )
                                                    m.users
                                                    |> Tuple.second
                                        in
                                        { m | users = newUsers }
                                   )

                        -- otherwise do nothing
                        _ ->
                            let
                                _ =
                                    Debug.log "no user to finalize" 123
                            in
                            model
            in
            ( newModel
            , getUserByUsername newModel.users username
                |> Maybe.map
                    (\u ->
                        Cmd.batch
                            [ Lamdera.sendToFrontend clientId <| NewUser u
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

        UserSentMessage chatContent ->
            getUserBySessionId model.users sessionId
                |> Maybe.andThen getUserData
                |> Maybe.map
                    (\userData ->
                        let
                            newMessage =
                                { userData = userData, message = chatContent, date = "" }

                            newAllChatMessages =
                                newMessage :: model.allChatMessages
                        in
                        ( { model | allChatMessages = newAllChatMessages }
                        , Lamdera.broadcast <| NewAllChatMessages newAllChatMessages
                        )
                    )
                |> Maybe.withDefault noop


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

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
        , Lamdera.onDisconnect OnClientDisconnect
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
            let
                newUsers =
                    model.users
                        |> List.Extra.updateIf
                            (\u ->
                                getSessionId u
                                    |> Maybe.map ((==) sessionId)
                                    |> Maybe.withDefault False
                            )
                            (\oldUser ->
                                mapFullUser
                                    (\ud ->
                                        FullUser { ud | isOnline = True }
                                    )
                                    oldUser
                            )

                newModel =
                    { model | users = newUsers }
            in
            ( newModel
            , Cmd.batch
                [ Lamdera.sendToFrontend clientId (NewTotalClicks newModel.totalClicks)
                , Lamdera.sendToFrontend clientId (NewTotalUsers <| List.length newModel.users)
                , case getUserBySessionId newModel.users sessionId of
                    Just user ->
                        Lamdera.sendToFrontend sessionId (NewUser user)

                    Nothing ->
                        Cmd.none
                , Lamdera.broadcast (NewUsernamesByPersonalityTypes (convertUsersToTeamsUserClicks newModel.users))
                , Lamdera.sendToFrontend clientId (NewClicksByPersonalityType newModel.teams)
                , Lamdera.sendToFrontend clientId (NewAllChatMessages newModel.allChatMessages)
                ]
            )

        OnClientDisconnect sessionId clientId ->
            let
                newUsers =
                    model.users
                        |> List.Extra.updateIf
                            (\u ->
                                getSessionId u
                                    |> Maybe.map ((==) sessionId)
                                    |> Maybe.withDefault False
                            )
                            (\oldUser ->
                                mapFullUser
                                    (\ud ->
                                        FullUser { ud | isOnline = False }
                                    )
                                    oldUser
                            )

                newModel =
                    { model | users = newUsers }
            in
            ( newModel
            , Lamdera.broadcast (NewUsernamesByPersonalityTypes (convertUsersToTeamsUserClicks newModel.users))
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


convertUsersToTeamsUserClicks : List User -> Types.TeamsUserClicks
convertUsersToTeamsUserClicks users =
    List.foldl
        (\user acc ->
            case user of
                AnonymousUser _ ->
                    acc

                PreppingUser _ _ ->
                    acc

                FullUser userData ->
                    let
                        toAdd =
                            { username = userData.username, clicks = userData.userClicks, isOnline = userData.isOnline }
                    in
                    case userData.personalityType of
                        Idealistic ->
                            { acc | idealists = toAdd :: acc.idealists }

                        Realistic ->
                            { acc | realists = toAdd :: acc.realists }
        )
        { realists = [], idealists = [] }
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


updateTeamByPersonalityType : Teams -> PersonalityType -> (Team -> Team) -> Teams
updateTeamByPersonalityType teams personalityType updater =
    case personalityType of
        Realistic ->
            updateRealists teams updater

        Idealistic ->
            updateIdealists teams updater


updateRealists : Teams -> (Team -> Team) -> Teams
updateRealists teams updater =
    { teams | realists = updater teams.realists }


updateIdealists : Teams -> (Team -> Team) -> Teams
updateIdealists teams updater =
    { teams | idealists = updater teams.idealists }


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

                            newTeams =
                                updateTeamByPersonalityType
                                    model.teams
                                    userData.personalityType
                                    (\t -> { t | totalTeamClicks = modifyClicks t.totalTeamClicks })
                                    |> --do the same lookup again, only pass through it the second time to check to new team points earned
                                       (\teams ->
                                            updateTeamByPersonalityType
                                                teams
                                                userData.personalityType
                                                (\t ->
                                                    if t.totalTeamClicks >= 100 then
                                                        { t | totalTeamClicks = t.totalTeamClicks - 100, totalTeamPoints = t.totalTeamPoints + 1 }

                                                    else
                                                        t
                                                )
                                       )

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
                                                    { ud | userClicks = modifyClicks ud.userClicks, xp = ud.xp + 1 }
                                                )
                                                |> Maybe.map FullUser
                                                |> Maybe.withDefault oldUser
                                        )

                            newModel : Model
                            newModel =
                                { model
                                    | totalClicks = modifyClicks model.totalClicks
                                    , teams = newTeams
                                    , users = newUsers
                                }
                        in
                        ( newModel
                        , Cmd.batch
                            [ Lamdera.broadcast (NewTotalClicks newModel.totalClicks)
                            , Lamdera.broadcast (NewClicksByPersonalityType newModel.teams)
                            , Lamdera.broadcast (NewUsernamesByPersonalityTypes (convertUsersToTeamsUserClicks newModel.users))
                            , Lamdera.sendToFrontend clientId (NewClicksByUser <| modifyClicks userData.userClicks)
                            , getUserBySessionId newModel.users sessionId
                                |> Maybe.map (Lamdera.sendToFrontend clientId << NewUser)
                                |> Maybe.withDefault Cmd.none
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

                            newTeams : Teams
                            newTeams =
                                model.teams
                                    |> --remove enemy teams clicks
                                       (\teams ->
                                            updateTeamByPersonalityType
                                                teams
                                                userData.personalityType
                                                (\team -> { team | totalTeamClicks = modifySelfClicks team.totalTeamClicks })
                                       )
                                    |> --remove enemy teams clicks
                                       (\teams ->
                                            updateTeamByPersonalityType
                                                teams
                                                (case userData.personalityType of
                                                    Realistic ->
                                                        Idealistic

                                                    Idealistic ->
                                                        Realistic
                                                )
                                                (\team -> { team | totalTeamClicks = modifyEnemyClicks team.totalTeamClicks })
                                       )

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
                                    , teams = newTeams
                                    , users = newUsers
                                }
                        in
                        ( newModel
                        , Cmd.batch
                            [ Lamdera.broadcast (NewTotalClicks newModel.totalClicks)
                            , Lamdera.broadcast (NewClicksByPersonalityType newModel.teams)
                            , Lamdera.broadcast (NewUsernamesByPersonalityTypes (convertUsersToTeamsUserClicks newModel.users))
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
                , Lamdera.broadcast (NewUsernamesByPersonalityTypes (convertUsersToTeamsUserClicks model.users))
                ]
            )

        UserFinalizedUser username ->
            let
                existingUserBySession =
                    getUserBySessionId model.users sessionId

                existingUserByUsername =
                    getUserByUsername model.users username

                newModel =
                    case ( existingUserBySession, existingUserByUsername ) of
                        -- if user with session id exists, replace it with a reset one
                        ( Just sessionUser, Nothing ) ->
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
                                                        , isOnline = True
                                                        , xp = 0
                                                        }

                                                FullUser userData ->
                                                    -- just replace session id, dont reset
                                                    FullUser
                                                        { sessionId = Just sessionId
                                                        , username = username
                                                        , personalityType = userData.personalityType
                                                        , userClicks = userData.userClicks
                                                        , isOnline = True
                                                        , xp = userData.xp
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
                                                                    , isOnline = True
                                                                    , xp = 0
                                                                    }

                                                            FullUser userData ->
                                                                -- override session id
                                                                FullUser
                                                                    { sessionId = Just sessionId
                                                                    , username = username
                                                                    , personalityType = userData.personalityType
                                                                    , userClicks = userData.userClicks
                                                                    , isOnline = True
                                                                    , xp = userData.xp
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
                            model
            in
            ( newModel
            , getUserByUsername newModel.users username
                |> Maybe.map
                    (\u ->
                        Cmd.batch
                            [ Lamdera.sendToFrontend clientId <| NewUser u
                            , Lamdera.broadcast (NewUsernamesByPersonalityTypes (convertUsersToTeamsUserClicks newModel.users))
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
                                        |> Maybe.map
                                            (\cid ->
                                                cid == sessionId
                                            )
                                        |> Maybe.withDefault False
                                )
                                (\u ->
                                    case u of
                                        AnonymousUser _ ->
                                            u

                                        PreppingUser sid pt ->
                                            toCreate

                                        FullUser userData ->
                                            FullUser { userData | sessionId = Nothing, isOnline = False }
                                )
                                model.users
                    }
            in
            ( newModel
            , Cmd.batch
                [ Lamdera.sendToFrontend sessionId <| NewUser toCreate
                , Lamdera.broadcast (NewUsernamesByPersonalityTypes (convertUsersToTeamsUserClicks newModel.users))
                ]
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

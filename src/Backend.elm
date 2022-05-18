module Backend exposing (..)

import ClickPricing
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
        -- , Time.every 1000 UpdateTick
        ]


init : ( Model, Cmd BackendMsg )
init =
    ( initBackendModel
    , Cmd.none
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
                , Lamdera.sendToFrontend clientId (NewTeams newModel.teams)
                , Lamdera.sendToFrontend clientId (NewAllChatMessages <| processChatMessages newModel.users newModel.allChatMessages)
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
            ( model, Cmd.none )


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
                                let
                                    numGroupMembers =
                                        Types.getGroupNumGroupMembers model.teams userData
                                            |> Maybe.withDefault 0

                                    extraClicks =
                                        ClickPricing.groupMemberClickBonus numGroupMembers
                                in
                                clicks + 1 + extraClicks

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
                            , Lamdera.broadcast (NewTeams newModel.teams)
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
                            , Lamdera.broadcast (NewTeams newModel.teams)
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
                    let
                        promoteUser sessionId_ personalityType =
                            --promote to full user
                            FullUser
                                { sessionId = Just sessionId_
                                , username = username
                                , personalityType = personalityType
                                , userClicks = 0
                                , isOnline = True
                                , xp = 0
                                , groupId = Nothing
                                , userId = generateUuid (username ++ sessionId_)
                                }

                        replaceUser existingUserData =
                            FullUser
                                { sessionId = Just sessionId
                                , username = username
                                , personalityType = existingUserData.personalityType
                                , userClicks = existingUserData.userClicks
                                , isOnline = True
                                , xp = existingUserData.xp
                                , groupId = existingUserData.groupId
                                , userId = existingUserData.userId
                                }

                        updateExistingUser newUser =
                            case newUser of
                                AnonymousUser _ ->
                                    newUser

                                PreppingUser sessionId_ personalityType ->
                                    promoteUser sessionId_ personalityType

                                FullUser userData ->
                                    -- just replace session id, dont reset
                                    replaceUser userData
                    in
                    case ( existingUserBySession, existingUserByUsername ) of
                        -- if user with session id exists, replace it with a reset one
                        ( Just sessionUser, Nothing ) ->
                            { model
                                | users =
                                    List.Extra.updateIf
                                        (userMatchesSessionId sessionId)
                                        updateExistingUser
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
                                                    updateExistingUser
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
                        , Lamdera.broadcast <| NewAllChatMessages <| processChatMessages model.users newAllChatMessages
                        )
                    )
                |> Maybe.withDefault noop

        UserWantsToBuyUpgrade upgradeType ->
            ( model, Cmd.none )

        UserWantsToJoinGroup groupUuid ->
            let
                maybeUserData =
                    getUserBySessionId model.users sessionId |> Maybe.andThen getUserData
            in
            case maybeUserData of
                Nothing ->
                    noop

                Just userData ->
                    let
                        team : Team
                        team =
                            Types.getTeamByPersonality model.teams userData.personalityType

                        maybeGroup =
                            List.Extra.find (.groupId >> (==) groupUuid) team.groups
                    in
                    case maybeGroup of
                        Nothing ->
                            noop

                        Just validGroup ->
                            let
                                --  update user's groupid
                                newUsers =
                                    updateFullUserByUsername
                                        model.users
                                        (\ud -> { ud | groupId = Just validGroup.groupId })
                                        userData.username

                                --  update group to contain user
                                newUserGroups : List Group -> List Group
                                newUserGroups groups =
                                    groups
                                        |> --remove user from all groups
                                           List.Extra.updateIf
                                            (.members >> List.member userData.userId)
                                            (\group -> { group | members = List.partition ((==) userData.userId) group.members |> Tuple.second })
                                        |> --add the new user
                                           List.Extra.updateIf
                                            (.groupId >> (==) validGroup.groupId)
                                            (\group -> { group | members = userData.userId :: group.members })

                                newRealistTeams : Team
                                newRealistTeams =
                                    model.teams.realists
                                        |> .groups
                                        |> newUserGroups
                                        |> setTeamGroups model.teams.realists

                                newIdealistTeams : Team
                                newIdealistTeams =
                                    model.teams.idealists
                                        |> .groups
                                        |> newUserGroups
                                        |> setTeamGroups model.teams.idealists

                                newTeams : Teams
                                newTeams =
                                    model.teams
                                        |> (\teams -> setRealistTeam teams newRealistTeams)
                                        |> (\teams -> setIdealistTeam teams newIdealistTeams)
                            in
                            ( { model | users = newUsers, teams = newTeams }
                            , -- broadcast user joining a new group
                              Cmd.batch
                                [ Lamdera.broadcast <| NewTeams newTeams
                                , getUserByUsername newUsers userData.username
                                    |> Maybe.map (Lamdera.sendToFrontend sessionId << NewUser)
                                    |> Maybe.withDefault Cmd.none
                                ]
                            )

        UserWantsToLeaveGroup ->
            let
                maybeUserData =
                    getUserBySessionId model.users sessionId |> Maybe.andThen getUserData
            in
            case maybeUserData of
                Nothing ->
                    noop

                Just userData ->
                    let
                        --  update user's groupid to Nothing
                        newUsers =
                            updateFullUserByUsername
                                model.users
                                (\ud -> { ud | groupId = Nothing })
                                userData.username

                        --  remove user from all groups
                        newUserGroups : List Group -> List Group
                        newUserGroups groups =
                            groups
                                |> --remove user from all groups
                                   List.Extra.updateIf
                                    (.members >> List.member userData.userId)
                                    (\group -> { group | members = List.partition ((==) userData.userId) group.members |> Tuple.second })

                        newRealistTeams : Team
                        newRealistTeams =
                            model.teams.realists
                                |> .groups
                                |> newUserGroups
                                |> setTeamGroups model.teams.realists

                        newIdealistTeams : Team
                        newIdealistTeams =
                            model.teams.idealists
                                |> .groups
                                |> newUserGroups
                                |> setTeamGroups model.teams.idealists

                        newTeams : Teams
                        newTeams =
                            model.teams
                                |> (\teams -> setRealistTeam teams newRealistTeams)
                                |> (\teams -> setIdealistTeam teams newIdealistTeams)
                    in
                    ( { model | users = newUsers, teams = newTeams }
                    , -- broadcast user joining a new group
                      Cmd.batch
                        [ Lamdera.broadcast <| NewTeams newTeams
                        , getUserByUsername newUsers userData.username
                            |> Maybe.map (Lamdera.sendToFrontend sessionId << NewUser)
                            |> Maybe.withDefault Cmd.none
                        ]
                    )


setTeamGroups : Team -> List Group -> Team
setTeamGroups team newUserGroups =
    { team | groups = newUserGroups }


setTeams : Model -> Teams -> Model
setTeams model newTeams =
    { model | teams = newTeams }


setRealistTeam : Teams -> Team -> Teams
setRealistTeam teams newRealistTeam =
    { teams | realists = newRealistTeam }


setIdealistTeam : Teams -> Team -> Teams
setIdealistTeam teams newIdealistTeam =
    { teams | idealists = newIdealistTeam }


{-| Only send 5 messages to client, and update the messages' user datas' isOnline
with the live data
-}
processChatMessages : List User -> List ChatMessage -> List ChatMessage
processChatMessages users allChatMessages =
    allChatMessages
        |> List.take 5
        |> List.map
            (\({ userData } as chatMessage) ->
                getUserByUsername users userData.username
                    |> --if user is found, get its user data
                       Maybe.andThen getUserData
                    |> --if the userdata is valid, replace the chatMessages's userdata with the user's userdata
                       Maybe.map (\ud -> { userData | isOnline = ud.isOnline })
                    |> -- then update the chat message's userdata with the updated userdata
                       Maybe.andThen (\ud -> Just { chatMessage | userData = ud })
                    |> --otherwise just return the original chatmessage
                       Maybe.withDefault chatMessage
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


updateFullUserByUsername : List User -> (UserData -> UserData) -> String -> List User
updateFullUserByUsername users updater username =
    users
        |> List.Extra.updateIf
            (\u ->
                getUsername u
                    |> Maybe.map ((==) username)
                    |> Maybe.withDefault False
            )
            (\oldUser ->
                mapUserData oldUser
                    updater
                    |> Maybe.map FullUser
                    |> Maybe.withDefault oldUser
            )

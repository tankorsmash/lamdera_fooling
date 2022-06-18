module Backend exposing (..)

import ClickPricing exposing (..)
import Dict
import Element
import Expect
import Lamdera exposing (ClientId, SessionId)
import List.Extra
import Process
import Random
import Random.List
import Task
import Test exposing (..)
import Test.Html.Query as Query
import Test.Html.Selector as Selector
import Time
import Types exposing (..)


type alias Model =
    BackendModel


app :
    { init : ( Model, Cmd BackendMsg )
    , update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
    , updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
    , subscriptions : Model -> Sub BackendMsg
    }
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
        , Time.every (1000 / 50) UpdateTick
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
                    updateFullUserBySessionId model.users
                        sessionId
                        (\ud ->
                            { ud | isOnline = True }
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
                    updateFullUserBySessionId model.users
                        sessionId
                        (\ud ->
                            { ud | isOnline = False }
                        )

                newModel =
                    { model | users = newUsers }
            in
            ( newModel
            , Lamdera.broadcast (NewUsernamesByPersonalityTypes (convertUsersToTeamsUserClicks newModel.users))
            )

        UpdateTick time ->
            ( { model | lastTick = time }, Cmd.none )


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


setUserClicks : Int -> UserData -> UserData
setUserClicks newClicks userData =
    { userData | userClicks = newClicks }


setXp : Int -> UserData -> UserData
setXp newXp userData =
    { userData | xp = newXp }


setCurrentLevels : CurrentLevels -> UserData -> UserData
setCurrentLevels newCurrentLevels userData =
    { userData | currentLevels = newCurrentLevels }


setDiscuss : CurrentLevels -> CurrentLevel -> CurrentLevels
setDiscuss currentLevels newDiscuss =
    { currentLevels | discuss = newDiscuss }


setArgue : CurrentLevels -> CurrentLevel -> CurrentLevels
setArgue currentLevels newArgue =
    { currentLevels | argue = newArgue }


setEnergize : CurrentLevels -> CurrentLevel -> CurrentLevels
setEnergize currentLevels newEnergize =
    { currentLevels | energize = newEnergize }


modifyTeamClicks : (Int -> Int) -> Team -> Team
modifyTeamClicks modifyClicks team =
    { team | totalTeamClicks = modifyClicks team.totalTeamClicks }


accumulateTeamPoints : Team -> Team
accumulateTeamPoints team =
    if team.totalTeamClicks >= 100 then
        { team | totalTeamClicks = team.totalTeamClicks - 100, totalTeamPoints = team.totalTeamPoints + 1 }

    else
        team


{-| broadcasts all the data required after a user gains a click of some sort

NOTE: doesn't update the user-specific stuff like sending the NewUser etc

-}
broadcastNewGlobalClicksAndSummaries : Model -> Cmd BackendMsg
broadcastNewGlobalClicksAndSummaries model =
    Cmd.batch
        [ Lamdera.broadcast (NewTotalClicks model.totalClicks)
        , Lamdera.broadcast (NewTeams model.teams)
        , Lamdera.broadcast (NewUsernamesByPersonalityTypes (convertUsersToTeamsUserClicks model.users))
        ]


{-| assumes a full user, since AnonymousUser and PreppingUsers can't gain clicks
-}
sendNewUserAfterClicksGained : Model -> SessionId -> ClientId -> Cmd BackendMsg
sendNewUserAfterClicksGained model sessionId clientId =
    Cmd.batch
        [ getUserBySessionId model.users sessionId
            |> Maybe.map
                (\user ->
                    case user of
                        AnonymousUser _ ->
                            Cmd.none

                        PreppingUser _ _ ->
                            Cmd.none

                        FullUser userData ->
                            Cmd.batch
                                [ Lamdera.sendToFrontend clientId <| NewUser user
                                , Lamdera.sendToFrontend clientId (NewClicksByUser <| userData.userClicks)
                                ]
                )
            |> Maybe.withDefault Cmd.none
        ]


updateWithNewClicksGained : Teams -> List User -> PersonalityType -> String -> Int -> ( Teams, List User )
updateWithNewClicksGained teams users personalityType username clicksToAdd =
    let
        modifyClicks =
            (+) clicksToAdd

        --modify the Teams Clicks
        newTeams =
            updateTeamByPersonalityType
                teams
                personalityType
                (modifyTeamClicks modifyClicks >> accumulateTeamPoints)

        --modify the users clicks
        newUsers =
            updateFullUserByUsername
                users
                (\ud ->
                    ud
                        |> setUserClicks (modifyClicks ud.userClicks)
                        |> setXp (ud.xp + 1)
                 -- TODO
                 -- |> setCurrentLevels (restartCurrentLevel ud.currentLevels)
                )
                username
    in
    ( newTeams, newUsers )


registerUserGainedAClick : Int -> UpdateData a -> UserData -> UpdateData a
registerUserGainedAClick clicksToAdd ({ teams, users, totalClicks } as updateData) userData =
    let
        clickCap : Int
        clickCap =
            userData.currentLevels.clickCap
                |> getCurrentLevelLevel
                |> basicBonuses.clickCap.clickBonus

        -- modifies whichever clicks get added
        modifyClicks : Int -> Int
        modifyClicks clicks =
            clicks + clicksToAdd

        --update the teams clicks
        newTeams : Teams
        newTeams =
            updateTeamByPersonalityType
                teams
                userData.personalityType
                (modifyTeamClicks modifyClicks >> accumulateTeamPoints)

        -- register the clicks for the user gaining them
        newUsers : List User
        newUsers =
            updateFullUserByUsername
                users
                (\ud ->
                    { ud
                        | userClicks = modifyClicks ud.userClicks |> min clickCap
                        , xp = ud.xp + 1
                    }
                )
                userData.username

        clicksChanged : Int
        clicksChanged =
            getUserByUsername newUsers userData.username
                |> Maybe.andThen getUserData
                |> Maybe.map
                    (.userClicks
                        >> (-) userData.userClicks
                        >> abs
                    )
                |> Maybe.withDefault 0
    in
    -- only add the clicks to the team and user, if there's been a change
    if clicksChanged > 0 then
        { updateData
            | totalClicks = modifyClicks totalClicks
            , teams = newTeams
            , users = newUsers
        }

    else
        updateData


type alias UpdateData a =
    { a | teams : Teams, users : List User, totalClicks : Int }


userGainedAClick : UpdateData a -> UserData -> UpdateData a
userGainedAClick ({ teams } as gameData) userData =
    let
        clicksToAdd : Int
        clicksToAdd =
            let
                numGroupMembers =
                    Types.getGroupNumGroupMembers teams userData
                        |> Maybe.withDefault 0

                extraClicks =
                    ClickPricing.groupMemberClickBonus numGroupMembers
            in
            1 + extraClicks
    in
    registerUserGainedAClick clicksToAdd gameData userData


userDiscussed : Model -> UserData -> Model
userDiscussed model userData =
    let
        clicksToAdd : Int
        clicksToAdd =
            userData.currentLevels.discuss
                |> getCurrentLevelLevel
                |> basicBonuses.discuss.clickBonus

        restartCurrentLevel : CurrentLevels -> CurrentLevels
        restartCurrentLevel currentLevels =
            mapCurrentLevels
                .discuss
                (\currentLevels_ newDiscussCurrentLevel ->
                    let
                        restartedDiscuss =
                            ClickPricing.currentLevelTimedRestarter
                                newDiscussCurrentLevel
                                model.lastTick
                                basicBonuses.discuss
                    in
                    setDiscuss currentLevels_ restartedDiscuss
                )
                currentLevels

        restartDiscuss : List User -> List User
        restartDiscuss users =
            updateFullUserByUsername
                users
                (\ud ->
                    setCurrentLevels (restartCurrentLevel ud.currentLevels) ud
                )
                userData.username
    in
    registerUserGainedAClick clicksToAdd model userData
        |> (\model_ -> mapUsers model_ restartDiscuss)


userArgued : Model -> UserData -> Model
userArgued model userData =
    let
        clicksToAdd : Int
        clicksToAdd =
            userData.currentLevels.argue
                |> getCurrentLevelLevel
                |> basicBonuses.argue.clickBonus

        restartCurrentLevel : CurrentLevels -> CurrentLevels
        restartCurrentLevel currentLevels =
            mapCurrentLevels
                .argue
                (\currentLevels_ newArgueCurrentLevel ->
                    let
                        restartedArgue =
                            ClickPricing.currentLevelTimedRestarter
                                newArgueCurrentLevel
                                model.lastTick
                                basicBonuses.argue
                    in
                    setArgue currentLevels_ restartedArgue
                )
                currentLevels

        restartArgue : List User -> List User
        restartArgue users =
            updateFullUserByUsername
                users
                (\ud ->
                    setCurrentLevels (restartCurrentLevel ud.currentLevels) ud
                )
                userData.username
    in
    registerUserGainedAClick clicksToAdd model userData
        |> (\model_ -> mapUsers model_ restartArgue)


mapUsers : Model -> (List User -> List User) -> Model
mapUsers model func =
    { model | users = func model.users }


type alias LevelManager =
    { getter : CurrentLevels -> CurrentLevel
    , setter : CurrentLevels -> CurrentLevel -> CurrentLevels
    , upgradeCost : Int
    }


upgradeUserCurrentLevels : Model -> SessionId -> ClientId -> UpgradeType -> { a | xp : Int } -> Maybe ( Model, Cmd msg )
upgradeUserCurrentLevels model sessionId clientId upgradeType userData =
    let
        updateWithNewUser newUsers =
            ( setUsersTo model newUsers
            , getUserBySessionId newUsers sessionId
                |> Maybe.map (\newUser -> Lamdera.sendToFrontend clientId <| NewUser newUser)
                |> Maybe.withDefault Cmd.none
            )

        setNextLevel : (CurrentLevels -> CurrentLevel -> CurrentLevels) -> CurrentLevels -> (CurrentLevel -> CurrentLevels)
        setNextLevel levelSetter currentLevels =
            nextCurrentLevel >> levelSetter currentLevels

        upgradeCurrentLevel : LevelManager -> CurrentLevels -> CurrentLevels
        upgradeCurrentLevel levelManager_ cls =
            ClickPricing.mapCurrentLevels levelManager_.getter (setNextLevel levelManager.setter) cls

        upgradeUsersCurrentLevel : LevelManager -> ( Model, Cmd msg )
        upgradeUsersCurrentLevel levelManager_ =
            let
                newUsers =
                    updateFullUserBySessionId
                        model.users
                        sessionId
                        (\ud ->
                            ud
                                |> setCurrentLevels (upgradeCurrentLevel levelManager_ ud.currentLevels)
                                |> setXp (ud.xp - levelManager_.upgradeCost)
                        )
            in
            updateWithNewUser newUsers

        canAffordUpgrade : { a | xp : Int } -> LevelManager -> Bool
        canAffordUpgrade { xp } { upgradeCost } =
            xp >= upgradeCost

        levelManager : LevelManager
        levelManager =
            case upgradeType of
                Types.Discussion level ->
                    { getter = .discuss
                    , setter = setDiscuss
                    , upgradeCost =
                        ClickPricing.xpCost ClickPricing.basicBonuses.discuss level
                    }

                Types.Argumentation level ->
                    { getter = .argue
                    , setter = setArgue
                    , upgradeCost =
                        ClickPricing.xpCost ClickPricing.basicBonuses.argue level
                    }

                Types.Energization level ->
                    { getter = .energize
                    , setter = setEnergize
                    , upgradeCost =
                        ClickPricing.xpCost ClickPricing.basicBonuses.energize level
                    }

                Types.EnergizeCap level ->
                    { getter = .energizeCycleCap
                    , setter = \cls nl -> { cls | energizeCycleCap = nl }
                    , upgradeCost =
                        ClickPricing.basicBonuses.energize.cycleCapUpgradeCost level
                    }

                Types.ClickCap level ->
                    { getter = .clickCap
                    , setter = \cls nl -> { cls | clickCap = nl }
                    , upgradeCost =
                        ClickPricing.xpCost ClickPricing.basicBonuses.clickCap level
                    }
    in
    if canAffordUpgrade userData levelManager then
        Just <| upgradeUsersCurrentLevel levelManager

    else
        Nothing


updateFromFrontend : SessionId -> SessionId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        NoOpToBackend ->
            noop

        UserGainedAClick ->
            getUserBySessionId model.users sessionId
                |> Maybe.andThen getUserData
                |> Maybe.map (userGainedAClick model)
                |> Maybe.map
                    (\newModel ->
                        ( newModel
                        , Cmd.batch
                            [ broadcastNewGlobalClicksAndSummaries newModel
                            , sendNewUserAfterClicksGained newModel sessionId clientId
                            ]
                        )
                    )
                |> Maybe.withDefault noop

        UserDiscussed ->
            getUserBySessionId model.users sessionId
                |> Maybe.andThen getUserData
                |> Maybe.map (userDiscussed model)
                |> Maybe.map
                    (\newModel ->
                        ( newModel
                        , Cmd.batch
                            [ broadcastNewGlobalClicksAndSummaries newModel
                            , sendNewUserAfterClicksGained newModel sessionId clientId
                            ]
                        )
                    )
                |> Maybe.withDefault noop

        UserArgued ->
            getUserBySessionId model.users sessionId
                |> Maybe.andThen getUserData
                |> Maybe.map (userArgued model)
                |> Maybe.map
                    (\newModel ->
                        ( newModel
                        , Cmd.batch
                            [ broadcastNewGlobalClicksAndSummaries newModel
                            , sendNewUserAfterClicksGained newModel sessionId clientId
                            ]
                        )
                    )
                |> Maybe.withDefault noop

        UserEnergized ->
            let
                withUserData userData =
                    let
                        currentLevelUpdater : CurrentLevels -> CurrentLevel -> ( CurrentLevels, Maybe Int )
                        currentLevelUpdater currentLevels energizeCurrentLevel =
                            let
                                energizeCapCurrentLevel =
                                    userData.currentLevels.energizeCycleCap

                                energizeCycleCap =
                                    basicBonuses.energize.cycleCap
                                        (ClickPricing.getCurrentLevelLevel energizeCapCurrentLevel)

                                energizeDuration =
                                    basicBonuses.energize.durationMs <|
                                        ClickPricing.getCurrentLevelLevel energizeCurrentLevel

                                addClickBonusToAvailableCycles =
                                    \availableCycles ->
                                        availableCycles
                                            * basicBonuses.energize.clickBonus
                                                -- FIXME? should this use the energizeCurrentLevel inside the tuple that its currently ignoring?
                                                (ClickPricing.getCurrentLevelLevel energizeCurrentLevel)

                                ( newCurrentLevel, gained ) =
                                    case getCurrentLevelProgress energizeCurrentLevel model.lastTick of
                                        NotStarted ->
                                            -- start the ticker
                                            ( ClickPricing.currentLevelTimedStarter
                                                energizeCurrentLevel
                                                model.lastTick
                                                basicBonuses.argue
                                            , Nothing
                                            )

                                        _ ->
                                            -- otherwise collect it
                                            ClickPricing.collectCurrentLevel
                                                energizeCurrentLevel
                                                model.lastTick
                                                energizeDuration
                                                energizeCycleCap
                                                |> Tuple.mapSecond (Maybe.map addClickBonusToAvailableCycles)
                            in
                            ( setEnergize currentLevels newCurrentLevel, gained )

                        -- diffs between the new user's userdata and original userData
                        modifyClicks existingClicks =
                            let
                                newClicks =
                                    (maybeNewUser
                                        |> Maybe.andThen getUserData
                                        |> Maybe.map .userClicks
                                        |> Maybe.withDefault userData.userClicks
                                    )
                                        - userData.userClicks
                            in
                            existingClicks
                                + newClicks

                        newTeams =
                            updateTeamByPersonalityType
                                model.teams
                                userData.personalityType
                                (modifyTeamClicks modifyClicks >> accumulateTeamPoints)

                        newUsers =
                            updateFullUserByUsername
                                model.users
                                (\ud ->
                                    let
                                        ( newCurrentLevels, maybeClicksGained ) =
                                            currentLevelUpdater
                                                ud.currentLevels
                                                ud.currentLevels.energize
                                    in
                                    { ud
                                        | currentLevels = newCurrentLevels
                                        , userClicks = Maybe.withDefault 0 maybeClicksGained + ud.userClicks
                                        , xp = ud.xp + 1
                                    }
                                )
                                userData.username

                        newModel : Model
                        newModel =
                            { model
                                | totalClicks = modifyClicks model.totalClicks
                                , users = newUsers
                                , teams = newTeams
                            }

                        maybeNewUser =
                            getUserBySessionId newUsers sessionId
                    in
                    ( newModel
                    , Cmd.batch
                        [ broadcastNewGlobalClicksAndSummaries newModel
                        , sendNewUserAfterClicksGained newModel sessionId clientId
                        ]
                    )
            in
            getUserBySessionId model.users sessionId
                |> Maybe.andThen getUserData
                |> Maybe.map withUserData
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

                            newUsers : List User
                            newUsers =
                                updateFullUserByUsername
                                    model.users
                                    (\ud ->
                                        { ud | userClicks = modifySelfClicks ud.userClicks }
                                    )
                                    userData.username

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
                            [ broadcastNewGlobalClicksAndSummaries newModel
                            , sendNewUserAfterClicksGained newModel sessionId clientId
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
                                    |> Maybe.map ((==) sessionId)
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
                            mapUserBySessionId model.users (always toCreate) sessionId
                                |> setUsersTo model

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
                            FullUser <|
                                createUserData (Just sessionId) username personalityType

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
                                , currentLevels = existingUserData.currentLevels
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
                toCreate =
                    AnonymousUser Nothing

                newModel =
                    mapUserBySessionId model.users
                        (\u ->
                            case u of
                                AnonymousUser _ ->
                                    u

                                PreppingUser _ _ ->
                                    toCreate

                                FullUser userData ->
                                    FullUser { userData | sessionId = Nothing, isOnline = False }
                        )
                        sessionId
                        |> setUsersTo model
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
                            newMessage : ChatMessage
                            newMessage =
                                { userData = userData
                                , message = chatContent
                                , --TODO date
                                  date = ""
                                , uuid = generateUuid (userData.username ++ chatContent)
                                }

                            newAllChatMessages =
                                newMessage :: model.allChatMessages
                        in
                        ( { model | allChatMessages = newAllChatMessages }
                        , Lamdera.broadcast <| NewAllChatMessages <| processChatMessages model.users newAllChatMessages
                        )
                    )
                |> Maybe.withDefault noop

        UserWantsToBuyUpgrade upgradeType ->
            getUserBySessionId model.users sessionId
                |> Maybe.andThen getUserData
                |> Maybe.andThen
                    (upgradeUserCurrentLevels
                        model
                        sessionId
                        clientId
                        upgradeType
                    )
                |> Maybe.withDefault noop

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
                                    (\group ->
                                        { group
                                            | members =
                                                List.partition
                                                    ((==) userData.userId)
                                                    group.members
                                                    |> Tuple.second
                                        }
                                    )

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

        AdminSendingToBackend adminToBackend ->
            updateFromAdminFrontend sessionId clientId adminToBackend model


updateFromAdminFrontend : SessionId -> SessionId -> Types.AdminToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromAdminFrontend sessionId clientId msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        NoOpAdminToBackend ->
            noop

        AdminWantsToDownloadUsers ->
            let
                usersToSend =
                    model.users
            in
            ( model
            , usersToSend
                |> DownloadedUsers
                |> NewToAdminFrontend
                |> Lamdera.sendToFrontend clientId
            )

        AdminWantsToDownloadChatMessages ->
            let
                usersToSend =
                    model.allChatMessages
            in
            ( model
            , usersToSend
                |> DownloadedChatMessages
                |> NewToAdminFrontend
                |> Lamdera.sendToFrontend clientId
            )

        AdminWantsToAddDummyUsers numUsers ->
            let
                ( newUsers, newSeed ) =
                    model.globalSeed
                        |> Random.step
                            (generateDummyUser
                                |> Random.map FullUser
                                |> Random.list numUsers
                            )
                        |> Tuple.mapFirst
                            ((++) model.users)
            in
            ( model
                |> setGlobalSeed newSeed
                |> setUsers newUsers
            , newUsers
                |> DownloadedUsers
                |> NewToAdminFrontend
                |> Lamdera.sendToFrontend clientId
            )


setGlobalSeed : Random.Seed -> Model -> Model
setGlobalSeed newSeed model =
    { model | globalSeed = newSeed }


dummyNames : List String
dummyNames =
    [ "Micheal", "José", "Alex", "Becca", "Charlie", "Debrah", "Elise", "Frank", "Gabrielle", "Liam", "Noah", "Oliver", "Elijah", "William", "James", "Benjamin", "Lucas", "Henry", "Alexander", "Mason", "Michael", "Ethan", "Daniel", "Jacob", "Logan", "Jackson", "Levi", "Sebastian", "Mateo", "Jack", "Owen", "Theodore", "Aiden", "Samuel", "Joseph", "John", "David", "Wyatt", "Matthew", "Luke", "Asher", "Carter", "Julian", "Grayson", "Leo", "Jayden", "Gabriel", "Isaac", "Lincoln", "Anthony", "Hudson", "Dylan", "Ezra", "Thomas", "Charles", "Christopher", "Jaxon", "Maverick", "Josiah", "Isaiah", "Andrew", "Elias", "Joshua", "Nathan", "Caleb", "Ryan", "Adrian", "Miles", "Eli", "Nolan", "Christian", "Aaron", "Cameron", "Ezekiel", "Colton", "Luca", "Landon", "Hunter", "Jonathan", "Santiago", "Axel", "Easton", "Cooper", "Jeremiah", "Angel", "Roman", "Connor", "Jameson", "Robert", "Greyson", "Jordan", "Ian", "Carson", "Jaxson", "Leonardo", "Nicholas", "Dominic", "Austin", "Everett", "Brooks", "Xavier", "Kai", "Jose", "Parker", "Adam", "Jace", "Wesley", "Kayden", "Silas" ]


generateDummyUser : Random.Generator UserData
generateDummyUser =
    let
        generateName : Random.Generator String
        generateName =
            Random.List.choose
                dummyNames
                |> Random.map
                    (Tuple.first
                        >> Maybe.withDefault "ImpossibleName"
                    )

        generatePersonalityType : Random.Generator PersonalityType
        generatePersonalityType =
            Random.List.choose
                [ Realistic, Idealistic ]
                |> Random.map
                    (Tuple.first
                        >> Maybe.withDefault Realistic
                    )

        generateExtraNumber : Random.Generator String
        generateExtraNumber =
            Random.int 0 1000
                |> Random.map
                    (String.fromInt >> (++) "_")
    in
    Random.map3
        (\name personalityType extraNumber ->
            createUserData
                Nothing
                (name ++ extraNumber)
                personalityType
        )
        generateName
        generatePersonalityType
        generateExtraNumber


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


setUsers : List User -> Model -> Model
setUsers users model =
    { model | users = users }


setUsersTo : Model -> List User -> Model
setUsersTo model users =
    { model | users = users }


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


mapUserByUsername : List User -> (User -> User) -> String -> List User
mapUserByUsername users updater username =
    users
        |> List.Extra.updateIf
            (\u ->
                getUsername u
                    |> Maybe.map ((==) username)
                    |> Maybe.withDefault False
            )
            updater


mapUserBySessionId : List User -> (User -> User) -> String -> List User
mapUserBySessionId users updater sessionId =
    users
        |> List.Extra.updateIf
            (\u ->
                getSessionId u
                    |> Maybe.map ((==) sessionId)
                    |> Maybe.withDefault False
            )
            updater


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


updateFullUserBySessionId : List User -> SessionId -> (UserData -> UserData) -> List User
updateFullUserBySessionId users sessionId updater =
    users
        |> List.Extra.updateIf
            (\u ->
                getSessionId u
                    |> Maybe.map ((==) sessionId)
                    |> Maybe.withDefault False
            )
            (\oldUser ->
                mapUserData oldUser
                    updater
                    |> Maybe.map FullUser
                    |> Maybe.withDefault oldUser
            )


suite : Test.Test
suite =
    let
        testUsername =
            "Testy McTesterson Jr."

        testUserData =
            createUserData Nothing testUsername Realistic

        testFullUser =
            FullUser testUserData

        testUsers =
            [ testFullUser ]

        testModel =
            { initBackendModel | users = testUsers }
    in
    describe "Test Suites"
        [ test "sample html test" <|
            \_ ->
                let
                    renderedHtml =
                        Element.layout [] (Element.text "hello")

                    query =
                        Query.fromHtml renderedHtml
                in
                Query.has
                    [ Selector.text "hello"
                    ]
                    query
        , test "getting user by username succeeds" <|
            \_ ->
                Expect.notEqual Nothing <|
                    getUserByUsername testUsers testUsername
        , test "getting user by username fails if its not a real username" <|
            \_ ->
                Expect.equal Nothing <|
                    getUserByUsername testUsers "ASDADSA ASD ADASAD AS ASD ASD"
        , describe "With Existing User"
            [ test "user gains a click" <|
                \_ ->
                    let
                        resultData =
                            userGainedAClick testModel testUserData
                    in
                    Expect.equal 1 resultData.totalClicks
            , test "user discusses" <|
                \_ ->
                    let
                        resultData =
                            userDiscussed testModel testUserData
                    in
                    Expect.equal 5 resultData.totalClicks
            , describe "click caps" <|
                let
                    alterUserData ud =
                        { ud | userClicks = clickCap }

                    alteredUsers =
                        updateFullUserByUsername
                            testModel.users
                            alterUserData
                            testUserData.username

                    clickCap =
                        testUserData.currentLevels.clickCap
                            |> getCurrentLevelLevel
                            |> basicBonuses.clickCap.clickBonus
                in
                [ test "user gains a tons of clicks but runs into the click limit without adding more clicks" <|
                    \_ ->
                        let
                            resultData =
                                userGainedAClick
                                    { testModel | totalClicks = clickCap, users = alteredUsers }
                                    (alterUserData testUserData)
                        in
                        Expect.equal clickCap resultData.totalClicks
                , test "user discusses and runs into the click limit without adding more clicks" <|
                    \_ ->
                        let
                            resultData =
                                userDiscussed
                                    { testModel | totalClicks = clickCap, users = alteredUsers }
                                    (alterUserData testUserData)
                        in
                        Expect.equal clickCap resultData.totalClicks
                , test "user argues and runs into the click limit without adding more clicks" <|
                    \_ ->
                        let
                            resultData =
                                userArgued
                                    { testModel | totalClicks = clickCap, users = alteredUsers }
                                    (alterUserData testUserData)
                        in
                        Expect.equal clickCap resultData.totalClicks
                ]
            ]
        ]

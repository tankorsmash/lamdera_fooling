module Backend exposing (..)

import Dict
import Html
import Lamdera exposing (SessionId)
import List.Extra
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


initModel : BackendModel
initModel =
    { message = "Hello!"
    , totalClicks = 0
    , clicksByPersonalityType =
        Dict.fromList
            [ ( "Idealistic", 0 )
            , ( "Realistic", 0 )
            ]
    , users = []
    }


init : ( Model, Cmd BackendMsg )
init =
    ( initModel
    , Cmd.none
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
                ]
            )


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
            case getUserBySessionId model.users sessionId of
                Just user ->
                    case user of
                        AnonymousUser _ ->
                            noop

                        PreppingUser _ _ ->
                            noop

                        FullUser userData ->
                            let
                                updatedClicksTracker =
                                    Dict.update
                                        (personalityTypeToDataId userData.personalityType)
                                        (Maybe.map ((+) 1))
                                        model.clicksByPersonalityType

                                newUsers =
                                    model.users
                                        |> List.Extra.updateIf
                                            (\u ->
                                                getUsername u
                                                    |> Maybe.map
                                                        (\username ->
                                                            username == userData.username
                                                        )
                                                    |> Maybe.withDefault False
                                            )
                                            (\oldUser ->
                                                mapUserData oldUser (\ud -> { ud | userClicks = ud.userClicks + 1 })
                                                    |> Maybe.map FullUser
                                                    |> Maybe.withDefault oldUser
                                            )

                                newModel : Model
                                newModel =
                                    { model
                                        | totalClicks = model.totalClicks + 1
                                        , clicksByPersonalityType = updatedClicksTracker
                                        , users = newUsers
                                    }
                            in
                            ( newModel
                            , Cmd.batch
                                [ Lamdera.broadcast (NewTotalClicks newModel.totalClicks)
                                , Lamdera.broadcast (NewClicksByPersonalityType newModel.clicksByPersonalityType)
                                , Lamdera.sendToFrontend clientId (NewClicksByUser <| userData.userClicks + 1)
                                ]
                            )

                Nothing ->
                    noop

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
            ( newModel, Lamdera.sendToFrontend sessionId (NewUser toCreate) )

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
                                            getSessionId u
                                                |> Maybe.map (\cid -> cid == sessionId)
                                                |> Maybe.withDefault False
                                        )
                                        (\u ->
                                            case u of
                                                AnonymousUser mbp ->
                                                    u

                                                PreppingUser sessionId_ personalityType ->
                                                    FullUser
                                                        { sessionId = Just sessionId_
                                                        , username = username
                                                        , personalityType = personalityType
                                                        , userClicks = 0
                                                        }

                                                FullUser userData ->
                                                    u
                                        )
                                        model.users
                            }

                        -- otherwise do nothing
                        Nothing ->
                            model
            in
            ( newModel
            , getUserBySessionId newModel.users sessionId
                |> Maybe.map (Lamdera.sendToFrontend sessionId << NewUser)
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

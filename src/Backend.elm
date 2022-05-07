module Backend exposing (..)

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


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!", totalClicks = 0, users = [] }
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
                [ Lamdera.sendToFrontend sessionId (NewTotalClicks model.totalClicks)
                , Lamdera.sendToFrontend sessionId (NewTotalUsers <| List.length model.users)
                , case getUserBySessionId model.users sessionId of
                    Just user ->
                        Lamdera.sendToFrontend sessionId (NewUser user)

                    Nothing ->
                        Cmd.none
                ]
            )


updateFromFrontend : SessionId -> SessionId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        UserGainedAClick ->
            let
                newModel : Model
                newModel =
                    { model | totalClicks = model.totalClicks + 1 }
            in
            ( newModel
            , Cmd.batch
                [ Lamdera.broadcast (NewTotalClicks newModel.totalClicks)
                ]
            )

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
                                                    FullUser { sessionId = Just sessionId_, username = username, personalityType = personalityType }

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

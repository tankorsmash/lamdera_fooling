module Backend exposing (..)

import Html
import Lamdera exposing (ClientId, SessionId)
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
    ( { message = "Hello!", clicks = 0, users = [] }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NoOpBackendMsg ->
            ( model, Cmd.none )

        OnClientConnect sessionId clientId ->
            ( model, Lamdera.sendToFrontend clientId (NewTotalClicks model.clicks) )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        NoOpToBackend ->
            ( model, Cmd.none )

        ToBackendClick ->
            let
                newModel : Model
                newModel =
                    { model | clicks = model.clicks + 1 }
            in
            ( newModel
            , Cmd.batch
                [ Lamdera.broadcast (NewTotalClicks newModel.clicks)
                ]
            )

        UserChoseToBe personalityType ->
            let
                existingUser =
                    model.users
                        |> List.filter
                            (\user ->
                                getClientId user
                                    |> Maybe.map (\cid -> cid == clientId)
                                    |> Maybe.withDefault False
                            )
                        |> List.head

                toCreate : User
                toCreate =
                    PreppingUser clientId personalityType

                newModel =
                    case existingUser of
                        -- if user exists, replace it
                        Just user ->
                            { model
                                | users =
                                    List.Extra.updateIf
                                        (\u ->
                                            getClientId u
                                                |> Maybe.map (\cid -> cid == clientId)
                                                |> Maybe.withDefault False
                                        )
                                        (always toCreate)
                                        model.users
                            }

                        -- otherwise add it
                        Nothing ->
                            { model | users = toCreate :: model.users }
            in
            ( model, Lamdera.sendToFrontend clientId (NewUser toCreate) )

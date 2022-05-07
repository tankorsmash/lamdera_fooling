module Backend exposing (..)

import Html
import Lamdera exposing (ClientId, SessionId)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \m -> Sub.none
        }


subscriptions : Model -> Sub BackendMsg
subscriptions model =
    Sub.batch
        [ Lamdera.onConnect OnClientConnect
        ]


init : ( Model, Cmd BackendMsg )
init =
    ( { message = "Hello!", clicks = 0 }
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

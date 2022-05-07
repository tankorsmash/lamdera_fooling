module Frontend exposing (Model, app, init, update, updateFromBackend, view)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Events
import Html.Attributes as Attr
import Lamdera
import Types exposing (FrontendModel, FrontendMsg(..), ToFrontend(..),  ToBackend(..))
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = UrlClicked
        , onUrlChange = UrlChanged
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( { key = key
      , message = "Now this is different"
      , clicksFromBackend = 0
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UrlClicked urlRequest ->
            case urlRequest of
                Internal url ->
                    ( model
                    , Nav.pushUrl model.key (Url.toString url)
                    )

                External url ->
                    ( model
                    , Nav.load url
                    )

        UrlChanged url ->
            ( model, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )

        SendClickToBackend ->
            ( model, Lamdera.sendToBackend ToBackendClick )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        NewTotalClicks totalClicks ->
            ( { model | clicksFromBackend = totalClicks}, Cmd.none)


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Testing Lamdera | TankorSmash"
    , body =
        [ Html.div [ Attr.style "text-align" "center", Attr.style "padding-top" "40px" ]
            [ Html.img [ Attr.src "https://lamdera.app/lamdera-logo-black.png", Attr.width 150 ] []
            , Html.div
                [ Attr.style "font-family" "sans-serif"
                , Attr.style "padding-top" "40px"
                ]
                [ Html.text model.message
                , Html.div [Html.Events.onClick SendClickToBackend] [ Html.text  "CLICK ME"]
                , Html.div [] [ Html.text  <| String.fromInt model.clicksFromBackend]
                ]
            ]
        ]
    }

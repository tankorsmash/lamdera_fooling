module Frontend exposing (Model, app, init, update, updateFromBackend, view)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Element
    exposing
        ( Color
        , Element
        , alignBottom
        , alignLeft
        , alignRight
        , alignTop
        , centerX
        , centerY
        , column
        , el
        , explain
        , fill
        , fillPortion
        , height
        , modular
        , padding
        , paddingXY
        , paragraph
        , rgb
        , rgb255
        , row
        , scrollbars
        , spacing
        , spacingXY
        , text
        , width
        )
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Html exposing (div, span)
import Html.Attributes as Attr
import Html.Events
import Interface as UI
import Lamdera
import Types exposing (FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend(..))
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
            ( { model | clicksFromBackend = totalClicks }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    let
        elm_ui_hack_layout =
            div [ Attr.style "height" "0" ]
                [ Element.layoutWith
                    { options =
                        [ Element.focusStyle
                            { borderColor = Nothing
                            , backgroundColor = Nothing
                            , shadow = Nothing
                            }
                        ]
                    }
                    [ Element.htmlAttribute <| Attr.id "hack" ]
                  <|
                    Element.none
                ]
    in
    { title = "Testing Lamdera | TankorSmash"
    , body =
        [ div
            []
            [ elm_ui_hack_layout
            , Html.br [] []
            , viewContent model
            ]
        ]
    }


viewContent : Model -> Html.Html FrontendMsg
viewContent model =
    Element.layoutWith
        { options =
            [ Element.noStaticStyleSheet
            , Element.focusStyle
                { borderColor = Nothing
                , backgroundColor = Nothing
                , shadow = Nothing
                }
            ]
        }
        [ width fill
        , height fill
        , padding 20
        , Element.htmlAttribute <| Attr.id "elm_ui_layout"
        ]
    <|
        column [ width fill, height fill, spacing 10 ]
            [ el [ centerX ] <| text <| "Clicks: " ++ String.fromInt model.clicksFromBackend
            , UI.button <|
                UI.TextParams
                    { buttonType = UI.Outline
                    , customAttrs =
                        [ centerX
                        , width Element.shrink
                        , Font.size 24
                        ]
                    , onPressMsg = SendClickToBackend
                    , textLabel = "Click Me"
                    , colorTheme = UI.BrightTheme
                    }
            ]

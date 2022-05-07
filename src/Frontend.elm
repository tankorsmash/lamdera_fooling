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
import Types exposing (FrontendModel, FrontendMsg(..), ToBackend(..), ToFrontend(..), User(..))
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


initModel : Nav.Key -> Model
initModel key =
    { key = key
    , message = "Now this is different"
    , clicksFromBackend = 0
    , user = AnonymousUser
    }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( initModel key
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
            , Element.layoutWith
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
                case model.user of
                    AnonymousUser ->
                        viewAnon model

                    PreppingUser ->
                        viewPlaying model

                    PreppedUser ->
                        viewPlaying model
            ]
        ]
    }


viewAnon : Model -> Element FrontendMsg
viewAnon model =
    let
        numRegisteredUsers =
            123

        currentlyActiveUsers =
            31

        viewChoice sideText =
            el
                [ alignLeft
                , width (fillPortion 1)
                , Element.pointer
                , Element.mouseOver [ Background.color <| UI.color_light_grey ]
                , Border.rounded 2
                , UI.noUserSelect
                ]
            <|
                paragraph [ padding 10 ] [ text sideText ]
    in
    column [ width fill, Font.center, height fill, spacing 10 ]
        [ paragraph [] [ text <| "You are a nobody." ]
        , paragraph []
            [ text <| "There are " ++ String.fromInt numRegisteredUsers ++ " people around you doing something."
            , text <| " And " ++ String.fromInt currentlyActiveUsers ++ " of them are even doing something right now."
            ]
        , paragraph [] [ text "Be somebody." ]

        -- choices
        , row [ centerX, width (fill |> Element.maximum 1000), padding 20 ]
            [ viewChoice "Be somebody you want to be"
            , el [ width (fillPortion 7) ] <| Element.none
            , viewChoice "Be somebody you never could be "
            ]

        -- extra text
        , paragraph [ Font.size <| UI.scaled 1 ] [ text <| "...pick a side so you can finally fit in" ]
        ]


viewPlaying : Model -> Element FrontendMsg
viewPlaying model =
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

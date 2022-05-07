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
import Element.Input as Input
import Html exposing (div, span)
import Html.Attributes as Attr
import Html.Events
import Interface as UI
import Lamdera
import Types exposing (FrontendModel, FrontendMsg(..), PersonalityType(..), ToBackend(..), ToFrontend(..), User(..))
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
    , user = AnonymousUser Nothing
    , username = ""

    -- , user = AnonymousUser (Just Idealistic)
    -- , user = AnonymousUser (Just Realistic)
    , totalUsers = 0
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
            ( model, Lamdera.sendToBackend UserGainedAClick )

        TryingOutPersonalityType personalityType ->
            ( { model | user = AnonymousUser personalityType }, Cmd.none )

        ResetPersonalityType ->
            ( { model | user = AnonymousUser Nothing }, Cmd.none )

        ConfirmedPersonalityType personalityType ->
            ( model, Lamdera.sendToBackend (UserChoseToBe personalityType) )

        ChangedUsername username ->
            ( { model | username = username }, Cmd.none )

        FinalizeUser ->
            ( model, Lamdera.sendToBackend <| UserFinalizedUser model.username )

        LogUserOut ->
            ( model, Lamdera.sendToBackend <| UserLoggedOut )



-- end of update


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        NewTotalClicks totalClicks ->
            ( { model | clicksFromBackend = totalClicks }, Cmd.none )

        NewUser user ->
            ( { model | user = user }, Cmd.none )

        NewTotalUsers totalUsers ->
            ( { model | totalUsers = totalUsers }, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    let
        elm_ui_hack_layout =
            div [ Attr.style "height" "0"]
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
        [ elm_ui_hack_layout
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
            , centerX
            ]
          <|
            case model.user of
                AnonymousUser maybePersonalityType ->
                    viewAnon model maybePersonalityType

                PreppingUser clientId personalityType ->
                    viewPrepping model personalityType

                FullUser { personalityType } ->
                    viewPlaying model personalityType
        ]
    }


viewPrepping : Model -> PersonalityType -> Element FrontendMsg
viewPrepping model personalityType =
    column [ centerX, Font.center, height fill, spacing 10 ]
        [ text <|
            (++) "You are " <|
                case personalityType of
                    Idealistic ->
                        "idealistic, and are going to have the best outcome possible."

                    Realistic ->
                        "realistic, and trying to make due with what you have."
        , text "What would they call you?"
        , Input.username [ width fill, centerX ]
            { onChange = ChangedUsername
            , text = model.username
            , placeholder =
                Just
                    (Input.placeholder []
                        (UI.monospace [] <|
                            text <|
                                case personalityType of
                                    Idealistic ->
                                        "AnIdeal1st321"

                                    Realistic ->
                                        "R3al1tyB1t3s"
                        )
                    )
            , label = Input.labelLeft [] <| text "You can be anyone..."
            }
        , text "You're going to have to click a lot no matter who you are."
        , if String.length model.username > 5 then
            el [ padding 30, centerX ] <|
                UI.button <|
                    UI.TextParams
                        { buttonType = UI.Secondary
                        , customAttrs =
                            [ centerX
                            , width Element.shrink
                            , Font.size 24
                            ]
                        , onPressMsg = FinalizeUser
                        , textLabel = "Are you sure?"
                        , colorTheme = UI.BrightTheme
                        }

          else
            Element.none
        ]


viewAnon : Model -> Maybe PersonalityType -> Element FrontendMsg
viewAnon model maybePersonalityType =
    let
        numRegisteredUsers =
            model.totalUsers

        currentlyActiveUsers =
            31

        viewChoice sideText personalityType =
            el
                [ alignLeft
                , width (fillPortion 1)
                , Element.pointer
                , Element.mouseOver [ Background.color <| UI.color_light_grey ]
                , Border.rounded 2
                , UI.noUserSelect
                , Events.onClick <|
                    case maybePersonalityType of
                        Nothing ->
                            TryingOutPersonalityType <| Just personalityType

                        Just currentPersonalityType ->
                            if currentPersonalityType == personalityType then
                                TryingOutPersonalityType Nothing

                            else
                                TryingOutPersonalityType <| Just personalityType
                , Background.color <|
                    (maybePersonalityType
                        |> Maybe.map
                            (\pt ->
                                if pt == personalityType then
                                    UI.color_light_grey

                                else
                                    UI.color_white
                            )
                        |> Maybe.withDefault UI.color_white
                    )
                ]
            <|
                paragraph [ padding 10 ] [ text sideText ]

        youAreText =
            maybePersonalityType
                |> Maybe.map
                    (\personalityType ->
                        case personalityType of
                            Idealistic ->
                                "idealistic person"

                            Realistic ->
                                "realistic person"
                    )
                |> Maybe.withDefault "nobody"

        realisticText =
            maybePersonalityType
                |> Maybe.map
                    (\personalityType ->
                        case personalityType of
                            Idealistic ->
                                "know you'd have to be if things didn't work out"

                            Realistic ->
                                "you know you have to be"
                    )
                |> Maybe.withDefault "you have to be"

        idealisticText =
            maybePersonalityType
                |> Maybe.map
                    (\personalityType ->
                        case personalityType of
                            Idealistic ->
                                "know you are going to be"

                            Realistic ->
                                "you know you don't have a chance to be"
                    )
                |> Maybe.withDefault "you want to be"
    in
    column [ width fill, Font.center, height fill, spacing 10 ]
        [ paragraph [] [ text <| "You are a " ++ youAreText ++ "." ]
        , paragraph []
            [ text <| "There are " ++ String.fromInt numRegisteredUsers ++ " people around you doing something."
            , text <| " And " ++ String.fromInt currentlyActiveUsers ++ " of them are even doing something right now."
            ]
        , paragraph []
            [ text <|
                "Be somebody"
                    ++ (maybePersonalityType
                            |> Maybe.map (always " else")
                            |> Maybe.withDefault ""
                       )
                    ++ "."
            ]

        -- choices
        , row [ centerX, width (fill |> Element.maximum 1000), padding 20 ]
            [ viewChoice ("Be somebody " ++ realisticText) Realistic
            , el [ width (fillPortion 3) ] <| Element.none
            , viewChoice ("Be somebody " ++ idealisticText) Idealistic
            ]

        -- extra text
        , case maybePersonalityType of
            Nothing ->
                paragraph [ Font.size <| UI.scaled 1 ]
                    [ text <|
                        "...pick a side so you can finally fit in"
                    ]

            Just personalityType ->
                UI.button <|
                    UI.TextParams
                        { buttonType = UI.Secondary
                        , customAttrs =
                            [ centerX
                            , width Element.shrink
                            , Font.size 24
                            ]
                        , onPressMsg = ConfirmedPersonalityType personalityType
                        , textLabel = "Are you sure?"
                        , colorTheme = UI.BrightTheme
                        }
        ]


viewPlaying : Model -> PersonalityType -> Element FrontendMsg
viewPlaying model personalityType =
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
        , UI.button <|
            UI.TextParams
                { buttonType = UI.Outline
                , customAttrs =
                    [ centerX
                    , width Element.shrink
                    , Font.size 24
                    , alignBottom
                    ]
                , onPressMsg = LogUserOut
                , textLabel = "Log me the heck out"
                , colorTheme = UI.BrightTheme
                }
        ]

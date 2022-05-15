module Frontend exposing (Model, app, init, update, updateFromBackend, view)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Dict
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
import Types exposing (ChatMessage, FrontendModel, FrontendMsg(..), PersonalityType(..), ToBackend(..), ToFrontend(..), User(..), getUsername, initFrontendModel, stringToPersonalityType)
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
    ( initFrontendModel key
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

        SendWantsToSpendToBackend ->
            ( model, Lamdera.sendToBackend UserWantsToSpend )

        TryingOutPersonalityType personalityType ->
            ( { model | user = AnonymousUser personalityType }, Cmd.none )

        ResetPersonalityType ->
            ( { model | user = AnonymousUser Nothing }, Cmd.none )

        ConfirmedPersonalityType personalityType ->
            ( model, Lamdera.sendToBackend (UserChoseToBe personalityType) )

        ChangedUsername newUsername ->
            ( { model | newUsername = newUsername }, Cmd.none )

        FinalizeUser ->
            ( { model | newUsername = "" }, Lamdera.sendToBackend <| UserFinalizedUser model.newUsername )

        LogUserOut ->
            ( model, Lamdera.sendToBackend <| UserLoggedOut )

        ChatInputChanged newMessage ->
            ( { model | userChatMessage = newMessage }, Cmd.none )

        ChatInputSent ->
            ( { model | userChatMessage = Nothing }
            , model.userChatMessage
                |> Debug.log "chat msg"
                |> Maybe.map (\chatMsg -> Lamdera.sendToBackend <| UserSentMessage chatMsg)
                |> Maybe.withDefault Cmd.none
            )



-- end of update


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        NewTotalClicks totalClicks ->
            ( { model | totalClicksFromBackend = totalClicks }, Cmd.none )

        NewClicksByPersonalityType clicksTracker ->
            ( { model | personalityTypeClicksFromBackend = clicksTracker }, Cmd.none )

        NewUser user ->
            ( { model | user = user }, Cmd.none )

        NewTotalUsers totalUsers ->
            ( { model | totalUsers = totalUsers }, Cmd.none )

        NewClicksByUser newClicks ->
            ( { model | userClicksFromBackend = newClicks }, Cmd.none )

        NewUsernamesByPersonalityTypes newUsernamesByPersonalityTypes ->
            ( { model | usernamesByPersonalityTypes = newUsernamesByPersonalityTypes }, Cmd.none )

        NewTick time ->
            ( model, Cmd.none )

        NewAllChatMessages allChatMessages ->
            ( { model | allChatMessages = allChatMessages }, Cmd.none )


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
    let
        finalizeMsg =
            if String.length model.newUsername > 3 then
                FinalizeUser

            else
                NoOpFrontendMsg
    in
    column [ centerX, Font.center, height fill, spacing 10 ]
        [ text <|
            (++) "You are " <|
                case personalityType of
                    Idealistic ->
                        "idealistic, and are going to have the best outcome possible."

                    Realistic ->
                        "realistic, and trying to make due with what you have."
        , text "What would they call you?"
        , Input.username [ width fill, centerX, UI.onEnter finalizeMsg ]
            { onChange = ChangedUsername
            , text = model.newUsername
            , placeholder =
                Just
                    (Input.placeholder [ Font.center, width fill ]
                        (UI.monospace [ Font.center, width fill ] <|
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
        , if String.length model.newUsername > 3 then
            el [ padding 30, centerX ] <|
                UI.button <|
                    UI.TextParams
                        { buttonType = UI.Secondary
                        , customAttrs =
                            [ centerX
                            , width Element.shrink
                            , Font.size 24
                            ]
                        , onPressMsg = finalizeMsg
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
                , height (fill |> Element.minimum 150)
                , centerY
                , Font.center
                ]
            <|
                paragraph [ Font.center, centerY, padding 10 ] [ text sideText ]

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
                el [ width fill ] <|
                    paragraph
                        [ Font.center
                        , Font.size <| UI.scaled 1
                        , centerY
                        , alignTop
                        , padding 10
                        ]
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


actionArea : Element FrontendMsg
actionArea =
    column [ centerX, width fill, spacing 10 ]
        [ el [ centerX, Font.underline ] <| text "Take action"
        , UI.button <|
            UI.TextParams
                { buttonType = UI.Outline
                , customAttrs =
                    [ centerX
                    , width Element.shrink
                    , Font.size 24
                    ]
                , onPressMsg = SendClickToBackend
                , textLabel = "Contribute"
                , colorTheme = UI.BrightTheme
                }
        , el [ centerX, Font.underline ] <| text "Spend your clicks"
        , UI.button <|
            UI.TextParams
                { buttonType = UI.Outline
                , customAttrs =
                    [ centerX
                    , width Element.shrink
                    , Font.size 24
                    ]
                , onPressMsg = SendWantsToSpendToBackend
                , textLabel = "-1 clicks"
                , colorTheme = UI.BrightTheme
                }
        , el [ centerX, Font.underline ] <| text "Spend your team's clicks"
        , UI.button <|
            UI.TextParams
                { buttonType = UI.Outline
                , customAttrs =
                    [ centerX
                    , width Element.shrink
                    , Font.size 24
                    ]
                , onPressMsg = SendClickToBackend
                , textLabel = "WIP"
                , colorTheme = UI.BrightTheme
                }
        ]


viewPlayers : Model -> Element FrontendMsg
viewPlayers model =
    let
        viewUsersInPersonalityType alliedPersonalityType =
            model.usernamesByPersonalityTypes
                |> Dict.get (Types.personalityTypeToDataId alliedPersonalityType)
                |> Maybe.map
                    (\names ->
                        let
                            header =
                                el [ Font.underline, paddingXY 0 5 ] <|
                                    (text <|
                                        "The "
                                            ++ Types.personalityTypeToDataId alliedPersonalityType
                                            ++ "s"
                                    )
                        in
                        column [ alignTop ] <|
                            (header
                                :: (names
                                        |> List.sortBy Tuple.second
                                        |> List.reverse
                                        |> List.map
                                            (\( name, count ) ->
                                                text <| name ++ " x" ++ String.fromInt count
                                            )
                                   )
                            )
                    )
                |> Maybe.withDefault Element.none
    in
    row [ width fill, centerX, Element.spaceEvenly ]
        [ viewUsersInPersonalityType Realistic
        , viewUsersInPersonalityType Idealistic
        ]


viewPlaying : Model -> PersonalityType -> Element FrontendMsg
viewPlaying model personalityType =
    column [ width fill, height fill, spacing 10 ]
        [ scoreboard model personalityType
        , column
            [ width fill
            , Element.behindContent <| viewPlayers model
            ]
            [ actionArea
            ]
        , bottomBar model.userChatMessage model.allChatMessages model.user personalityType
        ]


scoreboard : Model -> PersonalityType -> Element FrontendMsg
scoreboard model personalityType =
    let
        viewCountFromPersonality ( maybePersType, count ) =
            let
                strCount =
                    String.fromInt count
            in
            maybePersType
                |> Maybe.map
                    (\persType ->
                        if persType == personalityType then
                            text <| "Clicks from your people: " ++ strCount

                        else
                            text <| "Clicks from the other guys: " ++ strCount
                    )
                |> Maybe.withDefault
                    (text <| "Clicks from randos: " ++ strCount)
    in
    column [ width fill ]
        [ el [ centerX ] <| text <| "All clicks: " ++ String.fromInt model.totalClicksFromBackend
        , row [ centerX, spacing 10 ] <|
            (model.personalityTypeClicksFromBackend
                |> Dict.toList
                |> List.map
                    (Tuple.mapFirst stringToPersonalityType)
                |> List.sortWith
                    (\left right ->
                        Maybe.map2
                            (\l _ ->
                                if l == personalityType then
                                    LT

                                else
                                    GT
                            )
                            (Tuple.first left)
                            (Tuple.first right)
                            |> Maybe.withDefault EQ
                    )
                |> List.map
                    viewCountFromPersonality
            )
        , el [ centerX ] <| text <| "You've contributed: " ++ String.fromInt model.userClicksFromBackend
        ]


bottomBar : Maybe String -> List ChatMessage -> User -> PersonalityType -> Element FrontendMsg
bottomBar userChatMessage allChatMessages user personalityType =
    let
        viewChatMessage : ChatMessage -> Element FrontendMsg
        viewChatMessage chatMessage =
            paragraph [ Element.width (fill |> Element.maximum 500) ]
                [ el [ centerY, Font.center, UI.scaled_font 1, height fill ] <| text <| chatMessage.userData.username ++ ": "
                , el [ centerY, Font.center, height fill ] <| text <| chatMessage.message
                ]
    in
    column [ centerX, alignBottom, spacing 5 ]
        [ el [ centerX, UI.scaled_font 1, Font.color <| UI.color_light_grey ] <|
            text <|
                (getUsername user
                    |> Maybe.map identity
                    |> Maybe.withDefault ""
                )
        , row []
            [ Input.text [UI.onEnter ChatInputSent ]
                { label = Input.labelHidden "chat message input"
                , onChange = ChatInputChanged << Just
                , placeholder = Just <| Input.placeholder [] <| text "speak your mind"
                , text = userChatMessage |> Maybe.withDefault ""
                }
            , UI.button <|
                UI.TextParams
                    { buttonType = UI.Outline
                    , customAttrs =
                        [ width Element.shrink
                        , Font.size 24
                        , alignBottom
                        ]
                    , onPressMsg = ChatInputSent
                    , textLabel = "Send"
                    , colorTheme = UI.BrightTheme
                    }
            ]
        , column [] <|
            List.map viewChatMessage allChatMessages
        , UI.button <|
            UI.TextParams
                { buttonType = UI.Outline
                , customAttrs =
                    [ width Element.shrink
                    , Font.size 24
                    ]
                , onPressMsg = LogUserOut
                , textLabel = "Log me the heck out"
                , colorTheme = UI.BrightTheme
                }
        ]

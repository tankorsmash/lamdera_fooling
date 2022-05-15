module Frontend exposing (Model, app, init, update, updateFromBackend, view)

import Browser exposing (UrlRequest(..))
import Browser.Dom
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
import Process
import Task
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


focusChatInput : Cmd FrontendMsg
focusChatInput =
    focusElement "chat-message-input"


{-| delays a few ms to give it time to redraw, seems like 0ms and 1ms aren't long enough
-}
focusElement : String -> Cmd FrontendMsg
focusElement htmlId =
    Process.sleep 10
        |> Task.andThen
            (always (Browser.Dom.focus htmlId))
        |> Task.attempt FocusError


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
            let
                sendCmd : Cmd FrontendMsg
                sendCmd =
                    Lamdera.sendToBackend <| UserFinalizedUser model.newUsername

                focusCmd : Cmd FrontendMsg
                focusCmd =
                    focusChatInput
            in
            ( { model | newUsername = "" }
            , Cmd.batch
                [ sendCmd
                , focusCmd
                ]
            )

        LogUserOut ->
            ( model, Lamdera.sendToBackend <| UserLoggedOut )

        ChatInputChanged newMessage ->
            ( { model | userChatMessage = newMessage }, Cmd.none )

        ChatInputSent ->
            ( { model | userChatMessage = Nothing }
            , model.userChatMessage
                |> Maybe.map
                    (\chatMsg ->
                        Cmd.batch
                            [ Lamdera.sendToBackend <| UserSentMessage chatMsg
                            , focusChatInput
                            ]
                    )
                |> Maybe.withDefault Cmd.none
            )

        FocusError err ->
            ( model, Cmd.none )



-- end of update


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        NewTotalClicks totalClicks ->
            ( { model | totalClicksFromBackend = totalClicks }, Cmd.none )

        NewClicksByPersonalityType teams ->
            ( { model | teamsFromBackend = teams }, Cmd.none )

        NewUser user ->
            ( { model | user = user }, Cmd.none )

        NewTotalUsers totalUsers ->
            ( { model | totalUsers = totalUsers }, Cmd.none )

        NewClicksByUser newClicks ->
            ( { model | userClicksFromBackend = newClicks }, Cmd.none )

        NewUsernamesByPersonalityTypes newUsernamesByPersonalityTypes ->
            ( { model | teamsUserClicks = newUsernamesByPersonalityTypes }, Cmd.none )

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
            , UI.noUserSelect
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
        , Input.username [ width fill, centerX, UI.onEnter finalizeMsg, Input.focusedOnLoad ]
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
                , textLabel = "Contribute +1"
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
                , textLabel = "Spend -3 to reduce theirs by -1"
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
            model.teamsUserClicks
                |> (\tuc ->
                        case alliedPersonalityType of
                            Realistic ->
                                tuc.realists

                            Idealistic ->
                                tuc.idealists
                   )
                |> (\names ->
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
        viewCountFromPersonality teamPersonalityType team =
            if teamPersonalityType == personalityType then
                text <| "Clicks from your people: " ++ String.fromInt team.totalTeamClicks

            else
                text <| "Clicks from the other guys: " ++ String.fromInt team.totalTeamClicks
    in
    column [ width fill ]
        [ el [ centerX ] <| text <| "All clicks: " ++ String.fromInt model.totalClicksFromBackend
        , row [ centerX, spacing 10 ] <|
            [ viewCountFromPersonality Idealistic model.teamsFromBackend.idealists
            , viewCountFromPersonality Realistic model.teamsFromBackend.realists
            ]
        , paragraph [ centerX, Font.center ]
            [ text <| "You've contributed " ++ String.fromInt model.userClicksFromBackend ++ " clicks towards being better than those other guys."
            ]
        , paragraph [] [ el [ UI.scaled_font 1 ] <| text <| "Every 100 clicks from your team earns the team 1 point. Spend the points on upgrades to make your clicks worth more." ]
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
            [ Input.text [ UI.onEnter ChatInputSent, UI.defineHtmlId "chat-message-input" ]
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
            (allChatMessages
                |> List.take 5
                |> List.map viewChatMessage
            )
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

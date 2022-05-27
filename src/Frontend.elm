module Frontend exposing (Model, app, init, update, updateFromBackend, view)

import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation as Nav
import ClickPricing exposing (CurrentLevel, CurrentLevels, Level(..), Progress(..), addToLevel, basicBonuses, clickBonus, getCurrentLevelLevel, getCurrentLevelProgress, getLevel, groupMemberClickBonus, mapCurrentLevels, nextLevel, xpCost)
import Color
import Dict
import Duration
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, explain, fill, fillPortion, height, modular, padding, paddingXY, paragraph, rgb, rgb255, row, scrollbars, spacing, spacingXY, text, width)
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
import List.Extra
import Process
import Task
import Time
import Types
    exposing
        ( ChatMessage
        , FrontendModel
        , FrontendMsg(..)
        , Group
        , PersonalityType(..)
        , Team
        , Teams
        , ToBackend(..)
        , ToFrontend(..)
        , User(..)
        , UserData
        , getUserData
        , getUsername
        , initFrontendModel
        , stringToPersonalityType
        )
import UUID
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
        , subscriptions = subscriptions
        , view = view
        }


subscriptions : Model -> Sub FrontendMsg
subscriptions model =
    Time.every (1000 / 50) LocalTick


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
    let
        noop =
            ( model, Cmd.none )
    in
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

        LocalTick time ->
            ( { model | lastTick = time }, Cmd.none )

        SendClickToBackend ->
            ( model, Lamdera.sendToBackend UserGainedAClick )

        Discuss ->
            let
                maybeCurrentLevels =
                    model.user
                        |> getUserData
                        |> Maybe.map .currentLevels
            in
            case maybeCurrentLevels of
                Just currentLevels ->
                    let
                        prog =
                            Debug.log "prog" <| ClickPricing.getCurrentLevelProgress currentLevels.discuss model.lastTick
                    in
                    if prog == Completed || prog == NotStarted then
                        ( model, Lamdera.sendToBackend UserDiscussed )

                    else
                        -- TODO hopefully dont need to make a ui notification for the button being unclickable
                        Debug.log "unclickable " noop

                Nothing ->
                    -- TODO this should never happen, since the user is only able to do this if they're logged in
                    Debug.log "impossible" noop

        Argue ->
            let
                maybeCurrentLevels =
                    model.user
                        |> getUserData
                        |> Maybe.map .currentLevels
            in
            case maybeCurrentLevels of
                Just currentLevels ->
                    let
                        prog =
                            Debug.log "prog" <| ClickPricing.getCurrentLevelProgress currentLevels.argue model.lastTick
                    in
                    if prog == Completed || prog == NotStarted then
                        ( model, Lamdera.sendToBackend UserArgued )

                    else
                        -- TODO hopefully dont need to make a ui notification for the button being unclickable
                        Debug.log "unclickable " noop

                Nothing ->
                    -- TODO this should never happen, since the user is only able to do this if they're logged in
                    Debug.log "impossible" noop

        CollectEnergize ->
            let
                maybeCurrentLevels =
                    model.user
                        |> getUserData
                        |> Maybe.map .currentLevels
            in
            case maybeCurrentLevels of
                Just currentLevels ->
                    let
                        energizeLevel =
                            currentLevels.energize |> getCurrentLevelLevel

                        progress : Progress
                        progress =
                            ClickPricing.getCurrentLevelCycleProgress
                                currentLevels.energize
                                model.lastTick
                                (ClickPricing.bonusDuration basicBonuses.energize energizeLevel)

                        prog =
                            Debug.log "prog" <|
                                (ClickPricing.getCurrentLevelCycleCount currentLevels.energize
                                    model.lastTick
                                    (ClickPricing.bonusDuration basicBonuses.energize energizeLevel)
                                    |> Maybe.withDefault -123
                                )
                    in
                    if prog > 0 || progress == NotStarted then
                        ( model, Lamdera.sendToBackend UserEnergized )

                    else
                        -- TODO hopefully dont need to make a ui notification for the button being unclickable
                        Debug.log "unclickable " noop

                Nothing ->
                    -- TODO this should never happen, since the user is only able to do this if they're logged in
                    Debug.log "impossible" noop

        SendWantsToSpendToBackend ->
            ( model, Lamdera.sendToBackend UserWantsToSpend )

        TryingOutPersonalityType personalityType ->
            ( { model | user = AnonymousUser personalityType }, Cmd.none )

        ResetPersonalityType ->
            ( { model | user = AnonymousUser Nothing }, Cmd.none )

        ConfirmedPersonalityType personalityType ->
            ( model, Cmd.batch [ focusElement "set-username", Lamdera.sendToBackend (UserChoseToBe personalityType) ] )

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

        SendBuyUpgrade upgradeType ->
            ( model, Lamdera.sendToBackend (Types.UserWantsToBuyUpgrade upgradeType) )

        TryToJoinGroup groupUuid ->
            ( model, Lamdera.sendToBackend (Types.UserWantsToJoinGroup groupUuid) )

        TryToLeaveGroup ->
            ( model, Lamdera.sendToBackend Types.UserWantsToLeaveGroup )



-- end of update


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        NewTotalClicks totalClicks ->
            ( { model | totalClicksFromBackend = totalClicks }, Cmd.none )

        NewTeams teams ->
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

                FullUser userData ->
                    viewPlaying model userData
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
        , Input.username [ width fill, centerX, UI.onEnter finalizeMsg, UI.defineHtmlId "set-username" ]
            { onChange = ChangedUsername
            , text = model.newUsername
            , placeholder =
                Just
                    (Input.placeholder [ Font.center, width fill ]
                        (UI.monospace [ Font.center, width fill ] <|
                            text <|
                                case personalityType of
                                    Idealistic ->
                                        "AnIdeal1st2003"

                                    Realistic ->
                                        "xXDeadAirHangsXx"
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


{-| Only show an element if the bool is true
-}
showIf : Bool -> Element msg -> Element msg
showIf condition element =
    if condition then
        element

    else
        Element.none


viewProgressButton : Progress -> Int -> ( String, FrontendMsg ) -> Element FrontendMsg
viewProgressButton progress clicksOutput ( actionText, actionMsg ) =
    row [ width fill, spacing 10, height (fill |> Element.minimum 40) ]
        [ let
            buttonWidth =
                width (Element.px 90)
          in
          case progress of
            Completed ->
                UI.button <|
                    UI.TextParams
                        { buttonType = UI.Outline
                        , customAttrs =
                            [ centerX
                            , buttonWidth
                            , UI.scaled_font 2
                            ]
                        , onPressMsg = actionMsg
                        , textLabel = actionText
                        , colorTheme = UI.BrightTheme
                        }

            NotStarted ->
                UI.button <|
                    UI.TextParams
                        { buttonType = UI.Outline
                        , customAttrs =
                            [ centerX
                            , buttonWidth
                            , UI.scaled_font 2
                            ]
                        , onPressMsg = actionMsg
                        , textLabel = actionText
                        , colorTheme = UI.BrightTheme
                        }

            _ ->
                el [ buttonWidth, centerX, Font.center, UI.scaled_font 2 ] <|
                    text actionText
        , row
            [ width fill
            , height (fill |> Element.minimum 40)
            , padding 3
            , Element.inFront <|
                el
                    [ centerX
                    , centerY
                    , Background.color <|
                        UI.convertColor <|
                            case progress of
                                Completed ->
                                    Color.darkGreen

                                _ ->
                                    Color.lightBlue
                    , Border.rounded 3
                    , padding 2
                    ]
                <|
                    text <|
                        "+"
                            ++ (String.fromInt <| clicksOutput)
            ]
            (let
                sharedAttrs =
                    [ centerY, height fill ]

                emptyColor =
                    Background.color <| UI.convertColor <| Color.lightBlue

                filledColor =
                    Background.color <| UI.convertColor <| Color.darkBlue

                completedColor =
                    Background.color <| UI.convertColor <| Color.darkGreen
             in
             case progress of
                NotStarted ->
                    [ el (sharedAttrs ++ [ Border.rounded 3, width fill, emptyColor ]) <| Element.none
                    ]

                Progress p ->
                    let
                        filledIn =
                            10000 * ClickPricing.getProgress progress

                        empty =
                            10000 - filledIn
                    in
                    [ el (sharedAttrs ++ [ UI.borderRoundedLeft 3, width (Element.fillPortion <| round filledIn), filledColor ]) <| Element.none
                    , el (sharedAttrs ++ [ UI.borderRoundedRight 3, width (Element.fillPortion <| round empty), emptyColor ]) <| Element.none
                    ]

                Completed ->
                    [ el (sharedAttrs ++ [ Border.rounded 3, width fill, completedColor ]) <| Element.none
                    ]
            )
        ]


viewCycleButton : Progress -> Int -> ( String, FrontendMsg ) -> Element FrontendMsg
viewCycleButton progress clicksOutput ( actionText, actionMsg ) =
    row [ width fill, spacing 10, height (fill |> Element.minimum 40) ]
        [ let
            buttonWidth =
                width (Element.px 90)
          in
          if clicksOutput > 0 || progress == NotStarted then
            UI.button <|
                UI.TextParams
                    { buttonType = UI.Outline
                    , customAttrs =
                        [ centerX
                        , buttonWidth
                        , UI.scaled_font 2
                        ]
                    , onPressMsg = actionMsg
                    , textLabel = actionText
                    , colorTheme = UI.BrightTheme
                    }

          else
            el [ buttonWidth, centerX, Font.center, UI.scaled_font 2 ] <|
                text actionText
        , row
            [ width fill
            , height (fill |> Element.minimum 40)
            , padding 3
            , Element.inFront <|
                el
                    [ centerX
                    , centerY
                    , Background.color <|
                        UI.convertColor <|
                            case progress of
                                Completed ->
                                    Color.darkGreen

                                _ ->
                                    Color.lightBlue
                    , Border.rounded 3
                    , padding 2
                    ]
                <|
                    text <|
                        "+"
                            ++ (String.fromInt <| clicksOutput)
            ]
            (let
                sharedAttrs =
                    [ centerY, height fill ]

                emptyColor =
                    Background.color <| UI.convertColor <| Color.lightBlue

                filledColor =
                    Background.color <| UI.convertColor <| Color.darkBlue

                completedColor =
                    Background.color <| UI.convertColor <| Color.darkGreen
             in
             case progress of
                NotStarted ->
                    [ el (sharedAttrs ++ [ Border.rounded 3, width fill, emptyColor ]) <| Element.none
                    ]

                Progress p ->
                    let
                        filledIn =
                            10000 * ClickPricing.getProgress progress

                        empty =
                            10000 - filledIn
                    in
                    [ el (sharedAttrs ++ [ UI.borderRoundedLeft 3, width (Element.fillPortion <| round filledIn), filledColor ]) <| Element.none
                    , el (sharedAttrs ++ [ UI.borderRoundedRight 3, width (Element.fillPortion <| round empty), emptyColor ]) <| Element.none
                    ]

                Completed ->
                    [ el (sharedAttrs ++ [ Border.rounded 3, width fill, completedColor ]) <| Element.none
                    ]
            )
        ]


actionArea : Time.Posix -> Int -> Int -> CurrentLevels -> Element FrontendMsg
actionArea lastTick xp numGroupMembers currentLevels =
    let
        spacer =
            el [ padding 5 ] <| Element.none

        -- energizeCycleCap =
        --     ClickPricing.cycleCap basicBonuses.energizeCycleCap (getCurrentLevelLevel currentLevels.energizeCycleCap)
        --         |> Maybe.withDefault 10
    in
    column [ centerX, width fill, spacing 10 ]
        [ el [ centerX, Font.underline ] <| text <| "Take action (" ++ String.fromInt xp ++ "xp)"
        , UI.button <|
            UI.CustomParams
                { buttonType = UI.Outline
                , customAttrs =
                    [ centerX
                    , width Element.shrink
                    , Font.size 24
                    ]
                , onPressMsg = SendClickToBackend
                , customLabel =
                    row []
                        [ paragraph [ centerY, height fill ]
                            [ text "Contribute +1" ]
                        , el [ UI.scaled_font 2, Font.color <| UI.convertColor <| Color.lightBlue ] <|
                            text <|
                                if groupMemberClickBonus numGroupMembers > 0 then
                                    " +" ++ String.fromInt (groupMemberClickBonus numGroupMembers)

                                else
                                    ""
                        ]
                , colorTheme = UI.BrightTheme
                }
        , spacer
        , -- discuss
          let
            discussionLevel =
                currentLevels.discuss |> getCurrentLevelLevel
          in
          showIf (xp >= 10 || getLevel discussionLevel > 0) <|
            column [ centerX, width fill, spacing 10 ]
                [ viewProgressButton (getCurrentLevelProgress currentLevels.discuss lastTick) (clickBonus basicBonuses.discuss discussionLevel) ( "Discuss", Discuss )
                , UI.button <|
                    UI.TextParams
                        { buttonType = UI.Outline
                        , customAttrs =
                            [ centerX
                            , width Element.shrink
                            , UI.scaled_font 2
                            , Element.alpha <|
                                if xp >= xpCost basicBonuses.discuss (nextLevel discussionLevel) then
                                    1.0

                                else
                                    0.25
                            ]
                        , onPressMsg = SendBuyUpgrade (Types.Discussion <| nextLevel discussionLevel)
                        , textLabel = "Discussion +1 (" ++ String.fromInt (xpCost basicBonuses.discuss (addToLevel discussionLevel 1)) ++ "xp)"
                        , colorTheme = UI.BrightTheme
                        }
                ]
        , -- argue
          let
            argueLevel =
                currentLevels.argue |> getCurrentLevelLevel
          in
          showIf (xp >= 10 || (ClickPricing.getLevel argueLevel > 0)) <|
            column [ centerX, width fill, spacing 10 ]
                [ viewProgressButton (getCurrentLevelProgress currentLevels.argue lastTick) (clickBonus basicBonuses.argue argueLevel) ( "Argue", Argue )
                , UI.button <|
                    UI.TextParams
                        { buttonType = UI.Outline
                        , customAttrs =
                            [ centerX
                            , width Element.shrink
                            , UI.scaled_font 2
                            , Element.alpha <|
                                if xp >= xpCost basicBonuses.argue (nextLevel argueLevel) then
                                    1.0

                                else
                                    0.25
                            ]
                        , onPressMsg = SendBuyUpgrade (Types.Argumentation <| nextLevel argueLevel)
                        , textLabel = "Argumentation +1 (" ++ String.fromInt (xpCost basicBonuses.argue (addToLevel argueLevel 1)) ++ "xp)"
                        , colorTheme = UI.BrightTheme
                        }
                ]
        , let
            energizeLevel =
                currentLevels.energize |> getCurrentLevelLevel

            energizeCycleCapLevel =
                currentLevels.energizeCycleCap |> getCurrentLevelLevel
          in
          column [ centerX, width fill, spacing 10 ]
            [ viewCycleButton
                (ClickPricing.getCurrentLevelCycleProgress
                    currentLevels.energize
                    lastTick
                    (ClickPricing.bonusDuration basicBonuses.energize energizeLevel)
                )
                (ClickPricing.getAvailableCyclesCurrentLevel currentLevels.energize
                    lastTick
                    (ClickPricing.bonusDuration basicBonuses.energize energizeLevel)
                    |> Maybe.map
                        (min <|
                            (ClickPricing.cycleCap basicBonuses.energize energizeCycleCapLevel
                                |> Maybe.withDefault 10
                            )
                        )
                    |> Maybe.withDefault 0
                )
                ( "Energize", CollectEnergize )
            , row [ centerX, width fill, spacing 10 ]
                [ UI.button <|
                    UI.TextParams
                        { buttonType = UI.Outline
                        , customAttrs =
                            [ centerX
                            , width Element.shrink
                            , UI.scaled_font 2
                            , Element.alpha <|
                                if xp >= xpCost basicBonuses.energize (nextLevel energizeLevel) then
                                    1.0

                                else
                                    0.25
                            ]
                        , onPressMsg = SendBuyUpgrade (Types.Energization <| nextLevel energizeLevel)
                        , textLabel = "Energization +1 (" ++ String.fromInt (xpCost basicBonuses.energize (addToLevel energizeLevel 1)) ++ "xp)"
                        , colorTheme = UI.BrightTheme
                        }
                , let
                    upgradeXpCost =
                        ClickPricing.cycleCapUpgradeCost basicBonuses.energize (nextLevel energizeCycleCapLevel)
                            |> Maybe.withDefault -123

                    energizeCycleCap =
                        ClickPricing.cycleCap basicBonuses.energize energizeCycleCapLevel
                            |> Maybe.withDefault 10
                  in
                  UI.button <|
                    UI.TextParams
                        { buttonType = UI.Outline
                        , customAttrs =
                            [ centerX
                            , width Element.shrink
                            , UI.scaled_font 2
                            , Element.alpha <|
                                if xp >= upgradeXpCost then
                                    1.0

                                else
                                    0.25
                            ]
                        , onPressMsg = SendBuyUpgrade (Types.EnergizeCap <| nextLevel energizeCycleCapLevel)
                        , textLabel =
                            "Increase Cap to "
                                ++ (energizeCycleCap |> String.fromInt)
                                ++ " ("
                                ++ String.fromInt upgradeXpCost
                                ++ "xp)"
                        , colorTheme = UI.BrightTheme
                        }
                ]
            ]
        , spacer
        , -- convert Xp to clicks
          el [ centerX, Font.underline ] <| text "Spend your clicks"
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
        , spacer
        , el [ centerX, Font.underline ] <| text "Spend your team's points"
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


viewPlayers : Model -> UserData -> PersonalityType -> Element FrontendMsg
viewPlayers model userData personalityType =
    let
        maybeUserGroupId : Maybe Types.GroupId
        maybeUserGroupId =
            Maybe.map .groupId
                (Types.getUserGroup model.teamsFromBackend userData)

        viewUsersInPersonalityType =
            model.teamsUserClicks
                |> (\tuc ->
                        case personalityType of
                            Realistic ->
                                tuc.realists

                            Idealistic ->
                                tuc.idealists
                   )
                |> (\names ->
                        let
                            teamHeader =
                                el [ Font.underline, paddingXY 0 5 ] <|
                                    (text <|
                                        "The "
                                            ++ (case personalityType of
                                                    Realistic ->
                                                        "Realists (" ++ String.fromInt model.teamsFromBackend.realists.totalTeamPoints ++ " pts)"

                                                    Idealistic ->
                                                        "Idealists (" ++ String.fromInt model.teamsFromBackend.idealists.totalTeamPoints ++ " pts)"
                                               )
                                    )

                            viewGroup : Types.Group -> Element FrontendMsg
                            viewGroup group =
                                let
                                    ( headerColor, onClickMsg ) =
                                        maybeUserGroupId
                                            |> Maybe.map
                                                (\userGroupId ->
                                                    if userGroupId == group.groupId then
                                                        ( Font.color <| UI.convertColor <| Color.lightBlue, TryToLeaveGroup )

                                                    else
                                                        ( UI.noopAttr, TryToJoinGroup group.groupId )
                                                )
                                            |> Maybe.withDefault ( UI.noopAttr, TryToJoinGroup group.groupId )
                                in
                                column [ UI.scaled_font 1, paddingXY 0 5 ]
                                    [ el
                                        ([ Font.italic
                                         , headerColor
                                         ]
                                            ++ (if personalityType == userData.personalityType then
                                                    [ Element.pointer
                                                    , Events.onClick onClickMsg
                                                    ]

                                                else
                                                    []
                                               )
                                        )
                                      <|
                                        (text <| group.name)
                                    , text <| String.fromInt (List.length group.members) ++ " members"
                                    ]
                        in
                        column
                            [ alignTop
                            , UI.allowUserSelect
                            ]
                        <|
                            [ teamHeader
                            , column [ paddingXY 0 10 ] <|
                                text "Groups"
                                    :: (Types.getTeamByPersonality model.teamsFromBackend personalityType
                                            |> .groups
                                            |> List.map (\group -> viewGroup group)
                                       )
                            ]
                                ++ (names
                                        |> List.sortBy .clicks
                                        |> List.reverse
                                        |> List.map
                                            (\{ username, clicks, isOnline } ->
                                                if isOnline then
                                                    el [ Font.color <| UI.convertColor Color.lightGreen ] <|
                                                        text <|
                                                            (username ++ " x" ++ String.fromInt clicks)

                                                else
                                                    text <| username ++ " x" ++ String.fromInt clicks
                                            )
                                   )
                   )
    in
    viewUsersInPersonalityType


viewPlaying : Model -> Types.UserData -> Element FrontendMsg
viewPlaying model ({ personalityType, xp } as userData) =
    let
        numGroupMembers =
            Types.getGroupNumGroupMembers model.teamsFromBackend userData
                |> Maybe.withDefault 0
    in
    column [ width fill, height fill, spacing 10 ]
        [ scoreboard model personalityType
        , column [ width fill ]
            [ row [ width fill ]
                [ viewPlayers model userData Realistic
                , el [ centerX ] <|
                    actionArea model.lastTick xp numGroupMembers userData.currentLevels
                , viewPlayers model userData Idealistic
                ]
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
                [ el
                    ((if chatMessage.userData.isOnline then
                        [ Font.color <| UI.convertColor Color.lightGreen ]

                      else
                        []
                     )
                        ++ [ centerY, Font.center, UI.scaled_font 1, height fill ]
                    )
                  <|
                    text <|
                        (chatMessage.userData.username ++ ": ")
                , el [ centerY, Font.center, height fill ] <| text <| chatMessage.message
                ]
    in
    column [ centerX, alignBottom, spacing 5 ]
        [ el [ centerX, UI.scaled_font 1, Font.color <| UI.color_light_grey ] <|
            text <|
                (getUsername user
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
                    , centerX
                    ]
                , onPressMsg = LogUserOut
                , textLabel = "Log me the heck out"
                , colorTheme = UI.BrightTheme
                }
        ]

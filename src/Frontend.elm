module Frontend exposing (Model, app, init, update, updateFromBackend, view)

import AdminPage
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import ClickPricing exposing (CurrentLevel, CurrentLevels, Level(..), Progress(..), addToLevel, basicBonuses, getCurrentLevelLevel, getCurrentLevelProgress, getLevel, groupMemberClickBonus, mapCurrentLevels, nextLevel, xpCost)
import Color
import Dict
import Duration
import Easings
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, explain, fill, fillPortion, height, modular, padding, paddingXY, paragraph, rgb, rgb255, row, scrollbars, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import External.Animator.Animator as Animator
import Html exposing (div, span)
import Html.Attributes as Attr
import Html.Events
import Interface as UI
import Lamdera
import List.Extra
import Process
import String.Extra
import Task
import Time
import Types
    exposing
        ( ChatMessage
        , CyclingTimeline
        , FrontendModel
        , FrontendMsg(..)
        , Group
        , LabelValue(..)
        , PersonalityType(..)
        , Team
        , Teams
        , Timelines
        , ToBackend(..)
        , ToFrontend(..)
        , User(..)
        , UserData
        , getUserData
        , getUsername
        , initFrontendModel
        , setAdminFrontendModel
        , stringToPersonalityType
        )
import UUID
import Url
import Url.Parser as Parser exposing ((</>), Parser)


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
    Sub.batch
        [ Sub.none
        , timelineAnimator |> Animator.toSubscription LocalTick model
        , Browser.Events.onResize OnWindowResize
        ]


timelineAnimator : Animator.Animator Model
timelineAnimator =
    Animator.animator
        |> Animator.watchingWith
            (.timelines >> .userClicksTimeline)
            (\newTimeline ({ timelines } as m) ->
                let
                    newTimelines =
                        { timelines | userClicksTimeline = newTimeline }
                in
                { m | timelines = newTimelines }
            )
            (always True)
        |> Animator.watchingWith
            (.timelines >> .cyclingNumberTimeline >> Tuple.first)
            (\newTimeline ({ timelines } as m) ->
                let
                    newTimelines =
                        { timelines
                            | cyclingNumberTimeline =
                                ( newTimeline, Tuple.second timelines.cyclingNumberTimeline )
                        }
                in
                { m | timelines = newTimelines }
            )
            (always True)


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init url key =
    ( initFrontendModel url key
    , Task.perform
        (\viewport ->
            OnWindowResize
                (floor viewport.viewport.width)
                (floor viewport.viewport.height)
        )
        Browser.Dom.getViewport
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


setTimelines : Timelines -> Model -> Model
setTimelines timelines model =
    { model | timelines = timelines }


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
            let
                adminFrontendModel =
                    model.adminFrontendModel

                newAdminFrontendModel =
                    { adminFrontendModel | url = url }
            in
            ( { model | url = url, adminFrontendModel = newAdminFrontendModel }, Cmd.none )

        NoOpFrontendMsg ->
            noop

        LocalTick time ->
            ( { model | lastTick = time }
                |> Animator.update time timelineAnimator
            , Cmd.none
            )

        OnWindowResize width height ->
            ( { model | device = Just <| Element.classifyDevice { width = width, height = height } }
            , Cmd.none
            )

        SendClickToBackend ->
            model.user
                |> getUserData
                |> Maybe.map
                    (\userData ->
                        let
                            currentLevels =
                                userData.currentLevels

                            timelines =
                                model.timelines

                            clickCap =
                                currentLevels.clickCap
                                    |> getCurrentLevelLevel
                                    |> basicBonuses.clickCap.clickBonus

                            events =
                                let
                                    animatedValue =
                                        Animator.current timelines.userClicksTimeline
                                            |> (\state ->
                                                    case state of
                                                        Nothing ->
                                                            LabelNumber 1

                                                        Just (LabelNumber value) ->
                                                            LabelNumber (value + 1)

                                                        Just (LabelNumber2 value) ->
                                                            LabelNumber2 (value + 1)

                                                        Just (LabelString value) ->
                                                            LabelNumber 1
                                               )
                                            |> (\value ->
                                                    if userData.userClicks >= clickCap then
                                                        LabelString "Limit"

                                                    else
                                                        value
                                               )
                                            |> Just
                                in
                                [ Animator.event Animator.quickly animatedValue
                                , Animator.wait Animator.slowly
                                , Animator.event Animator.quickly Nothing
                                ]

                            newTimelines =
                                { timelines
                                    | userClicksTimeline =
                                        Animator.interrupt events model.timelines.userClicksTimeline
                                }
                        in
                        ( { model | timelines = newTimelines }
                        , Lamdera.sendToBackend UserGainedAClick
                        )
                    )
                |> Maybe.withDefault noop

        Discuss ->
            model.user
                |> getUserData
                |> Maybe.map .currentLevels
                |> Maybe.map
                    (\currentLevels ->
                        let
                            prog =
                                ClickPricing.getCurrentLevelProgress currentLevels.discuss model.lastTick
                        in
                        if prog == Completed || prog == NotStarted then
                            ( model, Lamdera.sendToBackend UserDiscussed )

                        else
                            -- TODO hopefully dont need to make a ui notification for the button being unclickable
                            noop
                    )
                |> Maybe.withDefault noop

        Argue ->
            model.user
                |> getUserData
                |> Maybe.map .currentLevels
                |> Maybe.map
                    (\currentLevels ->
                        let
                            prog =
                                ClickPricing.getCurrentLevelProgress currentLevels.argue model.lastTick
                        in
                        if prog == Completed || prog == NotStarted then
                            ( model, Lamdera.sendToBackend UserArgued )

                        else
                            -- TODO hopefully dont need to make a ui notification for the button being unclickable
                            noop
                    )
                |> Maybe.withDefault noop

        CollectEnergize ->
            model.user
                |> getUserData
                |> Maybe.map .currentLevels
                |> Maybe.map
                    (\currentLevels ->
                        let
                            energizeLevel =
                                currentLevels.energize |> getCurrentLevelLevel

                            progress : Progress
                            progress =
                                ClickPricing.getCurrentLevelCycleProgress
                                    currentLevels.energize
                                    model.lastTick
                                    (basicBonuses.energize.durationMs energizeLevel)

                            prog =
                                ClickPricing.getCurrentLevelCycleCount currentLevels.energize
                                    model.lastTick
                                    (basicBonuses.energize.durationMs energizeLevel)
                                    |> Maybe.withDefault -123
                        in
                        if prog > 0 || progress == NotStarted then
                            ( model, Lamdera.sendToBackend UserEnergized )

                        else
                            -- TODO hopefully dont need to make a ui notification for the button being unclickable
                            noop
                    )
                |> Maybe.withDefault noop

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

        GotAdminFrontendMsg adminFrontendMsg ->
            let
                ( newAdminFrontendModel, adminCmd ) =
                    AdminPage.update adminFrontendMsg model.adminFrontendModel
            in
            ( { model | adminFrontendModel = newAdminFrontendModel }
            , Cmd.map GotAdminFrontendMsg adminCmd
            )

        SendWantsToCraftXp numXp ->
            ( model, Lamdera.sendToBackend <| Types.UserWantsToCraftXp numXp )



-- end of update


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )

        NewTotalClicks totalClicks ->
            let
                timelines =
                    model.timelines

                newTimelines =
                    { timelines
                        | cyclingNumberTimeline =
                            timelines.cyclingNumberTimeline
                                |> (\( timeline, _ ) ->
                                        ( Animator.go Animator.quickly totalClicks timeline
                                        , Animator.current timeline
                                        )
                                   )
                    }
            in
            ( setTimelines newTimelines { model | totalClicksFromBackend = totalClicks }, Cmd.none )

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
            ( model
            , Cmd.none
            )

        NewAllChatMessages allChatMessages ->
            ( { model | allChatMessages = allChatMessages }, Cmd.none )

        NewToAdminFrontend toAdminFrontend ->
            let
                ( newAdminFrontendModel, adminCmd ) =
                    AdminPage.updateFromBackend toAdminFrontend model.adminFrontendModel
            in
            ( setAdminFrontendModel model newAdminFrontendModel, Cmd.map GotAdminFrontendMsg adminCmd )


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
                    let
                        route =
                            Parser.parse routeParser model.url
                                |> Maybe.withDefault GamePage
                    in
                    case route of
                        GamePage ->
                            viewGamePage model userData

                        AdminPage ->
                            viewAdminPage model userData
        ]
    }


viewAdminPage : Model -> UserData -> Element FrontendMsg
viewAdminPage model userData =
    Element.map GotAdminFrontendMsg <|
        AdminPage.view model.adminFrontendModel


type Route
    = GamePage
    | AdminPage


routeParser : Parser (Route -> a) a
routeParser =
    Parser.oneOf
        [ Parser.map GamePage Parser.top
        , Parser.map AdminPage (Parser.s "ules")
        ]


forceLabelValueToInt : LabelValue -> Int
forceLabelValueToInt labelValue =
    case labelValue of
        LabelString _ ->
            1

        LabelNumber number ->
            number

        LabelNumber2 number ->
            number


nbsp : String
nbsp =
    "\u{00A0}"


viewCyclingNumber : CyclingTimeline -> Element FrontendMsg
viewCyclingNumber ( timeline, oldNumber ) =
    let
        arrivedNumber =
            Animator.arrived timeline

        newNumber =
            Animator.current timeline

        oldStr =
            String.fromInt oldNumber

        newStr =
            String.fromInt newNumber

        digitLength =
            max
                (oldStr |> String.length)
                (newStr |> String.length)

        paddedNewStr =
            newStr
                |> String.padLeft digitLength '_'
                |> String.replace "_" nbsp

        paddedOldStr =
            oldStr
                |> String.padLeft digitLength '_'
                |> String.replace "_" nbsp

        moveDistance =
            20

        moveBy =
            (Animator.linear timeline <|
                \state ->
                    if state == oldNumber then
                        Animator.at 0.0

                    else
                        Animator.at 1.0
            )
                |> Easings.easeOutBack
                |> (*) moveDistance

        paddedZipped =
            List.Extra.zip
                (String.toList paddedOldStr)
                (String.toList paddedNewStr)

        viewAnimatedNewDigit =
            String.fromChar
                >> text
                >> el [ Element.moveDown (moveDistance - moveBy) ]

        viewAnimatedOldDigit =
            String.fromChar
                >> text
                >> el [ Element.moveUp moveBy ]

        viewStaticNewDigit =
            String.fromChar
                >> text
    in
    column []
        [ el [ Font.alignRight ]
            (paragraph
                [ Font.alignRight
                , Font.family [ Font.monospace ]
                , Element.clip
                ]
             <|
                (paddedZipped
                    |> List.map
                        (\( oldDigit, newDigit ) ->
                            el
                                (if oldDigit /= newDigit then
                                    [ Element.inFront <| viewAnimatedNewDigit newDigit
                                    , Element.inFront <| viewAnimatedOldDigit oldDigit
                                    ]

                                 else
                                    [ Element.inFront <| viewStaticNewDigit newDigit
                                    ]
                                )
                            <|
                                text "\u{00A0}"
                        )
                )
            )

        -- , UI.button <|
        --     UI.TextParams
        --         { buttonType = UI.Secondary
        --         , customAttrs =
        --             []
        --         , onPressMsg = NoOpFrontendMsg
        --         , textLabel = "Change"
        --         , colorTheme = UI.BrightTheme
        --         }
        ]


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


viewFlyingLabel : Animator.Timeline (Maybe LabelValue) -> Element FrontendMsg
viewFlyingLabel timeline =
    let
        labelHeight =
            Animator.move timeline <|
                \state ->
                    case state of
                        Just value ->
                            Animator.at 10

                        Nothing ->
                            Animator.at 0

        labelAlpha =
            Animator.move timeline <|
                \state ->
                    case state of
                        Just value ->
                            Animator.at 1.0

                        Nothing ->
                            Animator.at 0.0

        labelText =
            Animator.current timeline
                |> (\state ->
                        case state of
                            Nothing ->
                                " :)"

                            Just (LabelNumber value) ->
                                String.fromInt value |> (++) "+"

                            Just (LabelNumber2 value) ->
                                String.fromInt value |> (++) "+"

                            Just (LabelString value) ->
                                value
                   )
    in
    el
        [ Element.moveUp labelHeight
        , UI.font_grey
        , Element.alpha labelAlpha
        ]
    <|
        text labelText


actionArea : Element.DeviceClass -> Time.Posix -> Int -> Int -> UserData -> Timelines -> Element FrontendMsg
actionArea deviceClass lastTick xp numGroupMembers ({ currentLevels } as userData) timelines =
    let
        spacer =
            el [ padding 5 ] <| Element.none

        -- energizeCycleCap =
        --     ClickPricing.cycleCap basicBonuses.energizeCycleCap (getCurrentLevelLevel currentLevels.energizeCycleCap)
        --         |> Maybe.withDefault 10
        flyingLabel =
            viewFlyingLabel timelines.userClicksTimeline

        actionButton msg txt =
            actionButtonWithAttrs [] msg txt

        actionButtonWithAttrs attrs msg txt =
            UI.button <|
                UI.TextParams
                    { buttonType = UI.Outline
                    , customAttrs =
                        [ centerX
                        , width Element.shrink
                        , UI.scaled_font 3
                        ]
                            ++ attrs
                    , onPressMsg = msg
                    , textLabel = txt
                    , colorTheme = UI.BrightTheme
                    }

        rowOrColumn =
            case deviceClass of
                Element.Phone ->
                    column

                Element.Tablet ->
                    column

                Element.Desktop ->
                    row

                Element.BigDesktop ->
                    row

        contributeRow =
            row [ width fill, centerX, spacing 10 ]
                [ UI.button <|
                    UI.CustomParams
                        { buttonType = UI.Outline
                        , customAttrs =
                            [ centerX
                            , width Element.shrink
                            , UI.scaled_font 2
                            , Element.above <| flyingLabel
                            ]
                        , onPressMsg = SendClickToBackend
                        , customLabel =
                            row []
                                [ paragraph
                                    [ centerY
                                    , height fill
                                    ]
                                    [ text "Contribute +1" ]
                                , el
                                    [ UI.scaled_font 2
                                    , Font.color <| UI.convertColor <| Color.lightBlue
                                    ]
                                  <|
                                    text <|
                                        if groupMemberClickBonus numGroupMembers > 0 then
                                            " +" ++ String.fromInt (groupMemberClickBonus numGroupMembers)

                                        else
                                            ""
                                ]
                        , colorTheme = UI.BrightTheme
                        }
                , let
                    clickCapLevel =
                        currentLevels.clickCap
                            |> getCurrentLevelLevel

                    upgradeXpCost =
                        basicBonuses.clickCap.xpCost (nextLevel clickCapLevel)

                    clickCap =
                        basicBonuses.clickCap.clickBonus (nextLevel clickCapLevel)
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
                        , onPressMsg = SendBuyUpgrade (Types.ClickCap <| nextLevel clickCapLevel)
                        , textLabel =
                            "+Limit ["
                                ++ (clickCap |> String.fromInt)
                                ++ "] ("
                                ++ String.fromInt upgradeXpCost
                                ++ "xp)"
                        , colorTheme = UI.BrightTheme
                        }
                ]

        discussRow =
            -- discuss
            let
                discussionLevel =
                    currentLevels.discuss |> getCurrentLevelLevel
            in
            showIf (xp >= 10 || getLevel discussionLevel > 0) <|
                column [ centerX, width fill, spacing 10 ]
                    [ viewProgressButton (getCurrentLevelProgress currentLevels.discuss lastTick) (basicBonuses.discuss.clickBonus discussionLevel) ( "Discuss", Discuss )
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

        argueRow =
            -- argue
            let
                argueLevel =
                    currentLevels.argue |> getCurrentLevelLevel
            in
            showIf (xp >= 10 || (ClickPricing.getLevel argueLevel > 0)) <|
                column [ centerX, width fill, spacing 10 ]
                    [ viewProgressButton (getCurrentLevelProgress currentLevels.argue lastTick) (basicBonuses.argue.clickBonus argueLevel) ( "Argue", Argue )
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

        energizeRow =
            let
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
                        (basicBonuses.energize.durationMs energizeLevel)
                    )
                    (ClickPricing.getAvailableCyclesCurrentLevel currentLevels.energize
                        lastTick
                        (basicBonuses.energize.durationMs energizeLevel)
                        |> Maybe.map
                            (min <|
                                basicBonuses.energize.cycleCap energizeCycleCapLevel
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
                            basicBonuses.energize.cycleCapUpgradeCost (nextLevel energizeCycleCapLevel)

                        energizeCycleCap =
                            basicBonuses.energize.cycleCap energizeCycleCapLevel
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

        spendColumn =
            let
                buildCraftXpButton numXp =
                    let
                        level =
                            Level numXp

                        canAfford =
                            userData.userClicks >= basicBonuses.craftXp.xpCost level

                        buttonTextColor =
                            UI.optionalAttr (not canAfford) (Font.color UI.color_danger_dark)

                        clickCost =
                            basicBonuses.craftXp.xpCost level

                        xpGained =
                            basicBonuses.craftXp.clickBonus level
                    in
                    actionButtonWithAttrs [ buttonTextColor ] (SendWantsToCraftXp numXp) <|
                        String.fromInt xpGained
                            ++ "xp (-"
                            ++ String.fromInt clickCost
                            ++ " clicks)"
            in
            column [ centerX, spacing 10 ]
                [ el [ centerX, Font.underline ] <| text "Spend your clicks"
                , -- craft xp
                  row [ width fill, spacing 10 ]
                    [ text "Craft... "
                    , buildCraftXpButton 1
                    , buildCraftXpButton 5
                    , buildCraftXpButton 10
                    ]
                , spacer
                , actionButton SendWantsToSpendToBackend "Spend -3 clicks to reduce theirs by -1"
                , el [ centerX, Font.underline ] <| text "Spend your team's points"
                , actionButton SendClickToBackend "WIP"
                ]

        actionsColumn =
            column [ centerX, width fill, spacing 10 ]
                [ el [ centerX, Font.underline ] <| text "Take action"
                , el [centerX] <| text <| (String.fromInt xp ++ "xp")
                , contributeRow
                , spacer
                , discussRow
                , argueRow
                , energizeRow
                , spacer
                ]
    in
    column [ centerX, width fill, spacing 10 ] <|
        [ rowOrColumn [ centerX, width fill, spacing 40 ] <|
            [ actionsColumn
            , spendColumn
            ]
        ]


viewPlayers : Model -> UserData -> PersonalityType -> Element FrontendMsg
viewPlayers model userData personalityType =
    let
        maybeUserGroupId : Maybe Types.GroupId
        maybeUserGroupId =
            Maybe.map .groupId
                (Types.getUserGroup model.teamsFromBackend userData)

        viewUserInSidebar { username, clicks, isOnline } =
            let
                onlineAttrs =
                    if isOnline then
                        [ Font.color <| UI.convertColor Color.lightGreen ]

                    else
                        []

                usernameAttrs =
                    onlineAttrs
                        ++ (if username == userData.username then
                                [ Font.underline ]

                            else
                                []
                           )

                maxClicks =
                    userData.currentLevels.clickCap
                        |> getCurrentLevelLevel
                        |> ClickPricing.basicBonuses.clickCap.clickBonus

                usernameText =
                    el [ Element.htmlAttribute <| Attr.title username ] <|
                        text (username |> String.Extra.ellipsis 15)

                clicksContent =
                    if username /= userData.username then
                        text (" x" ++ String.fromInt clicks)

                    else
                        el
                            [ UI.optionalAttr (clicks >= maxClicks)
                                (Font.color <| UI.convertColor <| Color.lightRed)
                            ]
                            (text
                                (" x"
                                    ++ String.fromInt clicks
                                    ++ "/"
                                    ++ String.fromInt maxClicks
                                )
                            )
            in
            row [ width fill ] [ el usernameAttrs usernameText, clicksContent ]

        totalScoreText personalityType_ =
            let
                ( totalTeamPoints, personalityTypeString ) =
                    case personalityType_ of
                        Realistic ->
                            ( model.teamsFromBackend.realists.totalTeamPoints
                            , "Realists"
                            )

                        Idealistic ->
                            ( model.teamsFromBackend.idealists.totalTeamPoints
                            , "Idealists"
                            )
            in
            "The " ++ personalityTypeString ++ " (" ++ String.fromInt totalTeamPoints ++ " pts)"
    in
    model.teamsUserClicks
        |> (case personalityType of
                Realistic ->
                    .realists

                Idealistic ->
                    .idealists
           )
        |> (\names ->
                let
                    teamHeader =
                        el [ Font.underline, paddingXY 0 5 ] <|
                            (text <| totalScoreText personalityType)

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
                                    |> Maybe.withDefault
                                        ( UI.noopAttr, TryToJoinGroup group.groupId )
                        in
                        column
                            ([ UI.scaled_font 1
                             , paddingXY 0 5
                             ]
                                ++ UI.optionalAttrs
                                    (personalityType == userData.personalityType)
                                    [ Element.pointer
                                    , Events.onClick onClickMsg
                                    ]
                            )
                            [ el
                                [ Font.italic
                                , headerColor
                                ]
                              <|
                                (text <| group.name)
                            , List.length group.members
                                |> String.Extra.pluralize "member" "members"
                                |> text
                            ]
                in
                column
                    [ alignTop
                    , UI.allowUserSelect
                    , width fill
                    , height fill
                    ]
                <|
                    [ teamHeader
                    , column [ paddingXY 0 10 ] <|
                        text "Groups"
                            :: (Types.getTeamByPersonality model.teamsFromBackend personalityType
                                    |> .groups
                                    |> List.map viewGroup
                               )
                    , column
                        [ width fill
                        , height (fill |> Element.maximum 500)
                        , Element.clip
                        , Element.scrollbarY
                        ]
                      <|
                        (names
                            |> List.sortBy .clicks
                            |> List.reverse
                            |> List.map viewUserInSidebar
                        )
                    ]
           )


viewGamePage : Model -> Types.UserData -> Element FrontendMsg
viewGamePage model ({ personalityType, xp } as userData) =
    let
        numGroupMembers =
            Types.getGroupNumGroupMembers model.teamsFromBackend userData
                |> Maybe.withDefault 0

        deviceClass =
            model.device
                |> Maybe.map .class
                |> Maybe.withDefault Element.Desktop
    in
    column [ width fill, height fill, spacing 10 ]
        [ scoreboard model personalityType
        , row [ width fill ]
            [ row [ width fill, spacing 10 ]
                [ el [ alignLeft, height fill ] <| viewPlayers model userData Realistic
                , el [ centerX, alignTop ] <|
                    actionArea deviceClass model.lastTick xp numGroupMembers userData model.timelines
                , el [ alignRight, height fill ] <| viewPlayers model userData Idealistic
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
                text <| "Clicks from the others: " ++ String.fromInt team.totalTeamClicks
    in
    column [ width fill ]
        [ row [ centerX, width fill, Font.center ]
            [ el [ centerX ] <| text <| "Worldwide clicks: "
            , el [ centerX ] <| viewCyclingNumber model.timelines.cyclingNumberTimeline
            ]
        , row [ centerX, spacing 50 ] <|
            [ viewCountFromPersonality personalityType model.teamsFromBackend.idealists
            , viewCountFromPersonality (Types.otherPersonalityType personalityType) model.teamsFromBackend.realists
            ]
        , paragraph [ centerX, Font.center ]
            [ text <| "You've contributed "
            , el [ Font.underline ] <|
                text (String.fromInt model.userClicksFromBackend)
            , text " clicks towards being better than those other guys."
            ]
        , paragraph [ centerX, Font.center ] [ el [ UI.scaled_font 1 ] <| text <| "Every 100 clicks from your team earns the team 1 point. Spend the points on upgrades to make your clicks worth more." ]
        ]


bottomBar : Maybe String -> List ChatMessage -> User -> PersonalityType -> Element FrontendMsg
bottomBar userChatMessage allChatMessages user personalityType =
    let
        viewChatMessage : ChatMessage -> Element FrontendMsg
        viewChatMessage chatMessage =
            paragraph [ centerX, Element.width (fill |> Element.maximum 800) ]
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
    column [ width fill, centerX, alignBottom, spacing 5, UI.allowUserSelect ]
        [ el [ centerX, UI.scaled_font 1, Font.color <| UI.color_light_grey ] <|
            text <|
                (getUsername user
                    |> Maybe.withDefault ""
                )
        , --chat entry
          row [ centerX ]
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
        , --chat log
          column
            [ width Element.shrink
            , height (fill |> Element.minimum 200)
            , Element.scrollbarY
            , centerX
            ]
          <|
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

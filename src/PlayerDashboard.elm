module PlayerDashboard exposing (button, update, view)

import AdminPage
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Events
import Browser.Navigation as Nav
import ClickPricing exposing (CurrentLevel, CurrentLevels, Level(..), Progress(..), addToLevel, basicBonuses, getCurrentLevelLevel, getCurrentLevelProgress, getLevel, groupMemberClickBonus, mapCurrentLevels, nextLevel, xpCost)
import Color
import Color.Convert
import Color.Manipulate
import DateFormat.Relative exposing (relativeTime)
import Dict
import Duration
import Easings
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, explain, fill, fillPortion, height, modular, padding, paddingXY, paragraph, px, rgb, rgb255, row, scrollbars, spacing, spacingXY, text, width)
import Element.Background as Background
import Element.Border as Border
import Element.Events as Events
import Element.Font as Font
import Element.Input as Input
import Element.Lazy as Lazy
import External.Animator.Animator as Animator
import FontAwesome as FA
import FontAwesome.Attributes as FAA
import FontAwesome.Layering
import FontAwesome.Regular as FAR
import FontAwesome.Solid as FAS
import FontAwesome.Styles
import Html exposing (div, span)
import Html.Attributes as Attr
import Html.Events
import Interface as UI
import Lamdera
import List.Extra
import Password
import Process
import Random
import String.Extra
import Task
import Theme
import Time
import Types
    exposing
        ( ChatMessage
        , CyclingTimeline
        , DashboardModel
        , DashboardMsg(..)
        , DashboardTabType(..)
        , DashboardToBackend(..)
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
    DashboardModel


type alias Msg =
    DashboardMsg


clickableSidebarIcon : FA.Icon a -> msg -> Element msg
clickableSidebarIcon icon msg =
    el
        [ -- extra background size to click
          Element.behindContent <|
            el
                [ Element.scale 2
                , width fill
                , height fill
                , Element.pointer
                , Events.onClick msg
                ]
            <|
                Element.none
        ]
    <|
        el
            [ centerX
            , width (px 32)
            , height (px 32)
            , Events.onClick msg
            , Element.pointer
            ]
        <|
            UI.fontAwesome icon


viewSidebar : Model -> Element Msg
viewSidebar model =
    column
        [ Background.color <| Theme.purpleColor
        , height fill
        , padding 20
        , spacing 40
        , Border.rounded 30
        , Border.shadow
            { offset = ( 0, 6 )
            , size = 0
            , blur = 11
            , color = Theme.lightPurpleColor
            }
        , Font.color <| Theme.offWhiteColor
        ]
        [ el [ Theme.fontFamily "Roboto Slab" "https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@500&display=swap" ] <|
            text "Clikr"
        , column [ alignTop, centerX, spacing 20 ]
            [ clickableSidebarIcon FAR.hand (ChangeTab DashboardActionsTabType)
            , clickableSidebarIcon FAR.circleUp (ChangeTab DashboardUpgradesTabType)
            , clickableSidebarIcon FAR.faceFlushed NoOpDashboardFrontend
            ]
        ]


chatDivider : Element Msg
chatDivider =
    row [ width fill, padding 5 ]
        [ el [ width <| fillPortion 1 ] <| text ""
        , el
            [ width <| fillPortion 8
            , UI.border_bottom 1
            , paddingXY 10 0
            , centerX
            , Border.color Theme.borderColor
            ]
          <|
            text ""
        , el [ width <| fillPortion 1 ] <| text ""
        ]


viewChatMessage : Time.Posix -> ChatMessage -> Element Msg
viewChatMessage lastTick chatMessage =
    row [ width fill, spacing 10, UI.scaled_font 1 ]
        [ --profile pic
          el
            [ Background.color <| UI.hex_to_color "E4E5E7"
            , width (px 48)
            , height (px 48)
            , Border.rounded 100
            , --online dot
              Element.inFront <|
                UI.renderIf chatMessage.userData.isOnline <|
                    el
                        [ width (px 16)
                        , height (px 16)
                        , Background.color <| UI.color_pastel_green_4
                        , Border.rounded 100
                        , alignBottom
                        , alignRight
                        ]
                    <|
                        text " "
            ]
          <|
            text " "
        , --chat content
          column [ width fill, spacing 5 ]
            [ --user image
              paragraph [ Font.color Theme.darkHeaderColor, Theme.fontFamilyPoppins, UI.scaled_font 1 ] [ text <| chatMessage.userData.username ]
            , -- chat text
              paragraph [ width fill ] [ text <| chatMessage.message ]
            ]
        , --date
          column [ alignTop ]
            [ --NOTE not paragraph because we dont want the text to wrap
              el [ centerY, Font.size 10 ] <| text <| relativeTime lastTick chatMessage.date
            ]
        ]


viewChat : Maybe String -> List Types.ChatMessage -> Time.Posix -> Element Msg
viewChat userChatMessage allChatMessages lastTick =
    column [ width fill, height fill, paddingXY 0 10 ]
        [ --header
          row [ width fill, padding 10 ]
            [ el [ alignLeft, Font.bold, Font.color Theme.darkHeaderColor, Theme.fontFamilyPoppins ] <|
                text "All Chat"
            , el [ alignRight ] <| UI.fontAwesome FAR.comment
            ]
        , -- search bar
          el [ padding 5, width fill ] <|
            Input.text
                [ Border.rounded 30
                , Border.color Theme.borderColor
                , Background.color Theme.offWhiteBackgroundColor
                , paddingXY 20 5
                , width fill
                , UI.onEnter DashboardChatInputSent
                , UI.defineHtmlId "chat-message-input"
                , UI.scaled_font 1
                ]
                { onChange = Just >> Types.DashboardChatInputChanged
                , text = userChatMessage |> Maybe.withDefault ""
                , placeholder =
                    Just
                        (Input.placeholder
                            [ Font.color Theme.borderColor
                            , Element.moveRight 7
                            ]
                         <|
                            paragraph [ centerY ] [ text "Chat" ]
                        )
                , label = Input.labelHidden "search chat"
                }
        , column [ width fill, height fill, Element.scrollbarY, Font.color Theme.textColor, spacing 10, padding 10 ] <|
            (allChatMessages
                |> List.map (viewChatMessage lastTick)
                |> List.intersperse chatDivider
            )
        ]


verticalDivider : Element msg
verticalDivider =
    column [ height fill, width fill, paddingXY 5 0 ]
        [ el
            [ height fill
            , UI.border_right 1
            , centerX
            , Border.color Theme.borderColor
            ]
          <|
            text " "
        ]


view : List ChatMessage -> Time.Posix -> DashboardModel -> UserData -> Element Msg
view allChatMessages lastTick model userData =
    let
        actualView =
            row
                [ width fill
                , height fill
                , Background.color Theme.offWhiteBackgroundColor
                , Font.color Theme.textColor
                ]
                [ el [ height fill, paddingXY 30 20 ] <|
                    viewSidebar model
                , el [ width (fillPortion 3), height fill, paddingXY 0 20 ] <|
                    viewChat model.userChatMessage allChatMessages lastTick
                , el [ height fill, padding 15 ] <|
                    verticalDivider
                , el [ width (fillPortion 6), height fill, paddingXY 10 20 ] <|
                    case model.currentTabType of
                        DashboardActionsTabType ->
                            viewActions model userData lastTick

                        DashboardUpgradesTabType ->
                            viewUpgrades model userData

                        DashboardProfileTabType ->
                            viewProfile model userData
                ]
    in
    row [ width fill, height fill ]
        [ Element.html <| FontAwesome.Styles.css
        , actualView
        ]


viewProfile : DashboardModel -> UserData -> Element Msg
viewProfile model userData =
    column [ width fill, height fill, paddingXY 0 10, spacing 20 ]
        [ sectionHeader "Profile"
        , actionButtonWithAttrs [] LogUserOutFromDashboard "Log out"
        ]


viewUpgrades : DashboardModel -> UserData -> Element Msg
viewUpgrades model userData =
    column [ width fill, height fill, paddingXY 0 10, spacing 20 ]
        [ sectionHeader "Upgrades"
        , el [ alignLeft ] <|
            actionButtonWithAttrs [ width (px 150) ] NoOpDashboardFrontend "Craft XP"
        ]


actionButtonWithAttrs attrs msg txt =
    button
        ([ centerX
         , Font.size 16
         , Background.color Theme.buttonPrimaryColor
         ]
            ++ attrs
        )
        msg
        txt



-- primaryButtonConfig : ButtonConfig


primaryButtonConfig =
    { font_color = Theme.whiteFontColor
    , button_color = Theme.buttonPrimaryColor
    , hovered_button_color = Theme.buttonPrimaryHoveredColor
    , hovered_font_color = UI.color_white
    }



-- common_button_attrs : ButtonConfig -> List (Element.Attribute msg)


commonButtonAttrs { font_color, button_color, hovered_button_color, hovered_font_color } =
    [ -- bs4-like values
      Font.color font_color
    , Font.size 16
    , Font.center
    , padding 6
    , width (fill |> Element.minimum 150)
    , Background.color button_color
    , Border.rounded 15
    , Border.width 5
    , Border.color button_color
    , Element.mouseOver
        [ Background.color <| hovered_button_color
        , Border.color <| hovered_button_color
        , Font.color <| hovered_font_color
        ]
    , Border.shadow
        { offset = ( 4, 4 )
        , size = 0
        , blur = 5
        , color =
            UI.color_very_very_very_light_grey
        }
    ]


addButtonAttrs : List (Element.Attribute msg) -> List (Element.Attribute msg)
addButtonAttrs customAttrs =
    commonButtonAttrs primaryButtonConfig
        ++ customAttrs


button : List (Element.Attribute msg) -> msg -> String -> Element msg
button customAttrs onPressMsg textLabel =
    Input.button
        (addButtonAttrs customAttrs)
        { onPress = Just onPressMsg, label = text textLabel }


viewProgressButton : Progress -> Int -> ( String, msg ) -> Element msg
viewProgressButton progress clicksOutput ( actionText, actionMsg ) =
    let
        sharedAttrs =
            [ centerY, height fill ]

        emptyColor =
            Background.color <| UI.convertColor <| Color.Manipulate.lighten 0.2 <| Theme.rawPurpleColor

        filledColor =
            Background.color <| UI.convertColor <| Color.Manipulate.desaturate 0.35 <| Theme.rawPurpleColor

        completedColor =
            Background.color <| UI.convertColor <| Color.darkGreen
    in
    row [ width fill, spacing 10, height (fill |> Element.minimum 40) ]
        [ -- start/claim button
          let
            buttonWidth =
                width (Element.px 90)
          in
          case progress of
            Completed ->
                actionButtonWithAttrs [] actionMsg actionText

            NotStarted ->
                actionButtonWithAttrs [] actionMsg actionText

            Progress _ ->
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
                    , Font.size 14
                    , Background.color <|
                        case progress of
                            Completed ->
                                Theme.buttonPrimaryColor

                            _ ->
                                Theme.offWhiteBackgroundColor
                    , Border.rounded 30
                    , padding 5
                    , Font.color Theme.darkHeaderColor
                    , Theme.fontFamilyPoppins
                    ]
                <|
                    text ("+" ++ (String.fromInt <| clicksOutput))
            , Element.behindContent <|
                -- show progress bar behind so the rounded inner bar has a background
                el
                    (sharedAttrs
                        ++ [ Border.rounded 30
                           , width fill
                           , emptyColor
                           , Border.shadow
                                { offset = ( 4, 4 )
                                , size = 0
                                , blur = 5
                                , color =
                                    UI.color_very_very_very_light_grey
                                }
                           ]
                    )
                <|
                    Element.none
            ]
            -- render the progress bar
            (case progress of
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
                    [ el (sharedAttrs ++ [ Border.rounded 30, width (Element.fillPortion <| round filledIn), filledColor ]) <| Element.none
                    , el (sharedAttrs ++ [ UI.borderRoundedRight 30, width (Element.fillPortion <| round empty), emptyColor ]) <| Element.none
                    ]

                Completed ->
                    [ el (sharedAttrs ++ [ Border.rounded 3, width fill, completedColor ]) <| Element.none
                    ]
            )
        ]


discussAction : DashboardModel -> CurrentLevel -> Time.Posix -> Element Msg
discussAction model currentLevel lastTick =
    let
        progress =
            getCurrentLevelProgress currentLevel lastTick
    in
    row [ width fill ]
        [ viewProgressButton progress 99 ( "Discuss", NoOpDashboardFrontend )
        ]


argueAction : DashboardModel -> CurrentLevel -> Time.Posix -> Element Msg
argueAction model currentLevel lastTick =
    -- TODO use actual values
    let
        progress =
            getCurrentLevelProgress currentLevel lastTick
    in
    row [ width fill ]
        [ viewProgressButton progress 12 ( "Argue", NoOpDashboardFrontend )
        ]


energizeAction : DashboardModel -> CurrentLevel -> Time.Posix -> Element Msg
energizeAction model currentLevel lastTick =
    -- TODO use actual values
    let
        progress =
            getCurrentLevelProgress currentLevel lastTick
    in
    row [ width fill ]
        [ viewProgressButton progress 23 ( "Energize", NoOpDashboardFrontend )
        ]


sectionHeader : String -> Element Msg
sectionHeader headerTxt =
    row [ width fill, padding 10 ]
        [ el [ alignLeft, Font.bold, Font.color Theme.darkHeaderColor, Theme.fontFamilyPoppins ] <|
            text headerTxt
        , -- profile dropdown
          el [ alignRight, Events.onClick <| ChangeTab DashboardProfileTabType, Element.pointer ] <|
            row [ spacing 10 ]
                [ --TODO show xp and clicks here
                  el
                    [ Background.color <| UI.hex_to_color "E4E5E7"
                    , width (px 32)
                    , height (px 32)
                    , Border.rounded 100
                    ]
                  <|
                    text ""
                , el [ width (px 32) ] <| UI.fontAwesome <| FAS.chevronDown
                ]
        ]


viewActions : DashboardModel -> UserData -> Time.Posix -> Element Msg
viewActions model { currentLevels } lastTick =
    column [ width fill, height fill, paddingXY 0 10, spacing 20 ]
        [ -- header row
          sectionHeader "Actions"
        , column [ width fill, Element.paddingEach { top = 0, right = 40, bottom = 10, left = 0 }, spacing 20 ]
            [ discussAction model currentLevels.discuss lastTick
            , argueAction model currentLevels.argue lastTick
            , energizeAction model currentLevels.energize lastTick
            ]
        ]


{-| delays a few ms to give it time to redraw, seems like 0ms and 1ms aren't long enough
-}
focusElement : String -> Cmd Msg
focusElement htmlId =
    Process.sleep 10
        |> Task.andThen
            (always (Browser.Dom.focus htmlId))
        |> Task.attempt DashboardFocusError


focusChatInput : Cmd Msg
focusChatInput =
    focusElement "chat-message-input"


update : Types.DashboardMsg -> Model -> Types.UserData -> ( Model, Cmd Msg )
update msg model userData =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        NoOpDashboardFrontend ->
            noop

        ChangeTab tabType ->
            ( { model | currentTabType = tabType }, Cmd.none )

        LogUserOutFromDashboard ->
            -- the Frontend.elm is handling this, so we dont have to do anything
            noop

        DashboardChatInputChanged userChatMessage ->
            ( { model | userChatMessage = userChatMessage }, Cmd.none )

        DashboardChatInputSent ->
            let
                _ =
                    Debug.log "input sent" 123
            in
            ( { model | userChatMessage = Nothing }
            , model.userChatMessage
                |> Maybe.map
                    (\chatMsg ->
                        Cmd.batch
                            [ Lamdera.sendToBackend <|
                                (Types.DashboardSendingToBackend <|
                                    DashboardUserSentMessage chatMsg
                                )
                            , focusChatInput
                            ]
                    )
                |> Maybe.withDefault Cmd.none
            )

        DashboardFocusError error ->
            case error of
                Ok _ ->
                    noop

                Err (Browser.Dom.NotFound errText) ->
                    noop

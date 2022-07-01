module PlayerDashboard exposing (update, view)

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


fontFamily : String -> String -> Element.Attribute msg
fontFamily fontName fontUrl =
    Font.family
        [ Font.external
            { name = fontName
            , url = fontUrl
            }
        , Font.sansSerif
        ]


getRawColorFromHex : String -> Color.Color
getRawColorFromHex hexStr =
    Color.Convert.hexToColor hexStr
        |> Result.withDefault (Color.rgb 1 0 1)


rawPurpleColor : Color.Color
rawPurpleColor =
    getRawColorFromHex "6363FC"


purpleColor : Element.Color
purpleColor =
    rawPurpleColor
        |> UI.convertColor


lightPurpleColor =
    getRawColorFromHex "6363FC"
        |> Color.Manipulate.lighten 0.15
        |> UI.convertColor


offWhiteColor =
    UI.hex_to_color "F4F6FD"


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
            fontAwesome icon


viewSidebar : Model -> Element Msg
viewSidebar model =
    column
        [ Background.color <| purpleColor
        , height fill
        , padding 20
        , spacing 40
        , Border.rounded 30
        , Border.shadow
            { offset = ( 0, 6 )
            , size = 0
            , blur = 11
            , color = lightPurpleColor
            }
        , Font.color <| offWhiteColor
        ]
        [ el [ fontFamily "Roboto Slab" "https://fonts.googleapis.com/css2?family=Roboto+Slab:wght@500&display=swap" ] <|
            text "Clikr"
        , column [ alignTop, centerX, spacing 20 ]
            [ clickableSidebarIcon FAR.hand (ChangeTab DashboardActionsTabType)
            , clickableSidebarIcon FAR.circleUp (ChangeTab DashboardUpgradesTabType)
            , clickableSidebarIcon FAR.faceFlushed NoOpDashboardFrontend
            ]
        ]


exampleFontAwesomeLayeredIcon : Element msg
exampleFontAwesomeLayeredIcon =
    Element.html <|
        FontAwesome.Layering.layers [ FAA.lg ]
            [ FA.view FAS.checkToSlot
            , FontAwesome.Layering.counter [] "123"
            ]


fontAwesome : FA.Icon a -> Element msg
fontAwesome =
    FA.view >> Element.html >> el [ centerX, centerY, Font.center ]


offWhiteBackgroundColor : Element.Color
offWhiteBackgroundColor =
    UI.hex_to_color "F8FAFB"


whiteFontColor : Element.Color
whiteFontColor =
    UI.hex_to_color "F0F1FF"


darkHeaderColor : Element.Color
darkHeaderColor =
    UI.hex_to_color "191449"


textColor : Element.Color
textColor =
    UI.hex_to_color "837F98"


borderColor : Element.Color
borderColor =
    UI.hex_to_color "DFDFE1"


fontFamilyPoppins : Element.Attribute msg
fontFamilyPoppins =
    fontFamily
        "Poppins"
        "https://fonts.googleapis.com/css2?family=Poppins:wght@600&family=Roboto+Slab:wght@900&display=swap"


viewChat : Model -> List Types.ChatMessage -> Time.Posix -> Element Msg
viewChat model allChatMessages lastTick =
    let
        viewChatMessage : ChatMessage -> Element Msg
        viewChatMessage chatMessage =
            row [ width fill, spacing 10, UI.scaled_font 1 ]
                [ --profile pic
                  el
                    [ Background.color <| UI.hex_to_color "E4E5E7"
                    , width (px 48)
                    , height (px 48)
                    , Border.rounded 100
                    , --online dot
                      Element.inFront <|
                        UI.showIf chatMessage.userData.isOnline <|
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
                      paragraph [ Font.color darkHeaderColor, fontFamilyPoppins, UI.scaled_font 1 ] [ text <| chatMessage.userData.username ]
                    , -- chat text
                      paragraph [ width fill ] [ text <| chatMessage.message ]
                    ]
                , --date
                  column [ alignTop ]
                    [ --NOTE not paragraph because we dont want the text to wrap
                      el [ centerY, Font.size 10 ] <| text <| relativeTime lastTick chatMessage.date
                    ]
                ]

        chatDivider =
            row [ width fill, padding 5 ]
                [ el [ width <| fillPortion 1 ] <| text ""
                , el
                    [ width <| fillPortion 8
                    , UI.border_bottom 1
                    , paddingXY 10 0
                    , centerX
                    , Border.color borderColor
                    ]
                  <|
                    text ""
                , el [ width <| fillPortion 1 ] <| text ""
                ]
    in
    column [ width fill, height fill, paddingXY 0 10 ]
        [ --header
          row [ width fill, padding 10 ]
            [ el [ alignLeft, Font.bold, Font.color darkHeaderColor, fontFamilyPoppins ] <|
                text "All Chat"
            , el [ alignRight ] <| fontAwesome FAR.comment
            ]
        , -- search bar
          el [ padding 5, width fill ] <|
            Input.text
                [ Border.rounded 30
                , Border.color borderColor
                , Background.color offWhiteBackgroundColor
                , padding 3
                , width fill
                ]
                { onChange = always NoOpDashboardFrontend
                , text = ""
                , placeholder =
                    Just
                        (Input.placeholder
                            [ Font.color borderColor
                            , UI.scaled_font 1
                            , Element.moveRight 7
                            ]
                         <|
                            paragraph [ centerY ] [ text "Chat" ]
                        )
                , label = Input.labelHidden "search chat"
                }
        , column [ Font.color textColor, spacing 10, padding 10 ] <|
            List.intersperse chatDivider <|
                List.map viewChatMessage allChatMessages
        ]


verticalDivider : Element msg
verticalDivider =
    column [ height fill, width fill, paddingXY 5 0 ]
        [ el
            [ height fill
            , UI.border_right 1
            , centerX
            , Border.color borderColor
            ]
          <|
            text " "
        ]


view : FrontendModel -> DashboardModel -> UserData -> Element Msg
view tempFrontendModel model userData =
    let
        actualView =
            row
                [ width fill
                , height fill
                , Background.color offWhiteBackgroundColor
                , Font.color textColor
                ]
                [ el [ height fill, paddingXY 30 20 ] <|
                    viewSidebar model
                , el [ width (fillPortion 5), height fill, paddingXY 0 20 ] <|
                    viewChat model tempFrontendModel.allChatMessages tempFrontendModel.lastTick
                , el [ height fill, padding 15 ] <|
                    verticalDivider
                , el [ width (fillPortion 6), height fill, paddingXY 10 20 ] <|
                    case model.currentTabType of
                        DashboardActionsTabType ->
                            viewActions model userData

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


buttonPrimaryColor : Element.Color
buttonPrimaryColor =
    UI.hex_to_color "6F6AF8"


buttonPrimaryHoveredColor : Element.Color
buttonPrimaryHoveredColor =
    Color.Convert.hexToColor "6F6AF8"
        |> Result.withDefault Color.red
        |> Color.Manipulate.lighten 0.05
        |> Color.toRgba
        |> UI.rgbaToColor


actionButtonWithAttrs attrs msg txt =
    button
        ([ centerX
         , Font.size 16
         , Background.color buttonPrimaryColor
         ]
            ++ attrs
        )
        msg
        txt



-- primaryButtonConfig : ButtonConfig


primaryButtonConfig =
    { font_color = whiteFontColor
    , button_color = buttonPrimaryColor
    , hovered_button_color = buttonPrimaryHoveredColor
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
            Background.color <| UI.convertColor <| Color.Manipulate.lighten 0.2 <| rawPurpleColor

        filledColor =
            Background.color <| UI.convertColor <| Color.Manipulate.desaturate 0.35 <| rawPurpleColor

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
                                buttonPrimaryColor

                            _ ->
                                offWhiteBackgroundColor
                    , Border.rounded 30
                    , padding 5
                    , Font.color darkHeaderColor
                    , fontFamilyPoppins
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


discussAction : DashboardModel -> Element Msg
discussAction model =
    -- TODO use actual values
    row [ width fill ]
        [ actionButtonWithAttrs [] NoOpDashboardFrontend "Discuss"
        , viewProgressButton (ClickPricing.Progress 0.45) 99 ( "", NoOpDashboardFrontend )
        ]


argueAction : DashboardModel -> Element Msg
argueAction model =
    -- TODO use actual values
    row [ width fill ]
        [ actionButtonWithAttrs [] NoOpDashboardFrontend "Argue"
        , viewProgressButton (ClickPricing.Progress 0.95) 12 ( "", NoOpDashboardFrontend )
        ]


energizeAction : DashboardModel -> Element Msg
energizeAction model =
    -- TODO use actual values
    row [ width fill ]
        [ actionButtonWithAttrs [] NoOpDashboardFrontend "Energize"
        , viewProgressButton (ClickPricing.Progress 0.25) 23 ( "", NoOpDashboardFrontend )
        ]


sectionHeader : String -> Element Msg
sectionHeader headerTxt =
    row [ width fill, padding 10 ]
        [ el [ alignLeft, Font.bold, Font.color darkHeaderColor, fontFamilyPoppins ] <|
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
                , el [ width (px 32) ] <| fontAwesome <| FAS.chevronDown
                ]
        ]



-- TODO generate this on the model, since we'll need to generate a new one
-- viewHash : String -> Element Msg
-- viewHash password =
--     Password.generateHash password (Random.initialSeed 12345)
--         |> Tuple.first
--         >> Result.map Password.getHash
--         |> Result.withDefault ""
--         |> Debug.toString
--         |> text
--


viewActions : DashboardModel -> UserData -> Element Msg
viewActions model userData =
    column [ width fill, height fill, paddingXY 0 10, spacing 20 ]
        [ -- header row
          sectionHeader "Actions"
        , column [ width fill, Element.paddingEach { top = 0, right = 40, bottom = 10, left = 0 }, spacing 20 ]
            [ discussAction model
            , argueAction model
            , energizeAction model
            ]
        ]


update : Types.DashboardMsg -> Model -> Types.UserData -> ( Model, Cmd msg )
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

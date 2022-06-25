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
import Process
import String.Extra
import Task
import Time
import Types
    exposing
        ( ChatMessage
        , CyclingTimeline
        , DashboardModel
        , DashboardMsg(..)
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


viewSidebar : Model -> Element Msg
viewSidebar model =
    let
        purpleColor =
            UI.hex_to_color "6363FC"

        lightPurpleColor =
            Color.Convert.hexToColor "6363FC"
                |> Result.withDefault Color.red
                |> Color.Manipulate.lighten 0.15
                |> Color.toRgba
                |> UI.rgbaToColor

        offWhiteColor =
            UI.hex_to_color "F4F6FD"
    in
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
            [ el [ centerX, height (px 32) ] <|
                fontAwesome FAR.hand
            , el [ centerX, height (px 32) ] <|
                fontAwesome FAR.circleUp
            , el [ centerX, height (px 32) ] <|
                fontAwesome FAR.faceGrinBeamSweat
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
    FA.view >> Element.html


offWhiteBackgroundColor : Element.Color
offWhiteBackgroundColor =
    UI.hex_to_color "F8FAFB"


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
                            paragraph [ centerY ] [ text "Search" ]
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


view : FrontendModel -> DashboardModel -> Element Msg
view tempFrontendModel model =
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
                    viewActions model
                ]
    in
    row [ width fill, height fill ]
        [ Element.html <| FontAwesome.Styles.css
        , actualView
        ]


viewActions : DashboardModel -> Element Msg
viewActions model =
    column [ width fill, height fill, paddingXY 0 10 ]
        [ row [ width fill, padding 10 ]
            [ el [ alignLeft, Font.bold, Font.color darkHeaderColor, fontFamilyPoppins ] <|
                text "Actions"
            ]
        ]


update : Types.DashboardMsg -> Model -> ( Model, Cmd c )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        NoOpDashboardFrontend ->
            noop

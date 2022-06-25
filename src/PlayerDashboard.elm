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


viewChat : Model -> Element Msg
viewChat model =
    column [ width fill, height fill, paddingXY 20 10 ]
        [ row [ width fill ]
            [ el [ alignLeft, Font.bold, fontFamily "Poppins" "https://fonts.googleapis.com/css2?family=Poppins:wght@600&family=Roboto+Slab:wght@900&display=swap" ] <| text "All Chat"
            , el [ alignRight ] <| fontAwesome FAR.comment
            ]
        , text "Chat"
        ]


view : DashboardModel -> Element Msg
view model =
    let
        actualView =
            row
                [ width fill
                , height fill
                , Background.color offWhiteBackgroundColor
                , padding 20
                ]
                [ el [ width (fillPortion 1), height fill ] <| viewSidebar model
                , el [ width (fillPortion 3), height fill ] <| viewChat model
                , el [ width (fillPortion 6), height fill ] <| text "Dashboard"
                ]
    in
    row [ width fill, height fill ]
        [ Element.html <| FontAwesome.Styles.css
        , actualView
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

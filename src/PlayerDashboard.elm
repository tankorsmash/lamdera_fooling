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
    in
    column
        [ Background.color <| purpleColor
        , height fill
        , padding 20
        , Border.rounded 30
        , Border.shadow
            { offset = ( 0, 6 )
            , size = 0
            , blur = 11
            , color = lightPurpleColor
            }
        ]
        [ text "sidebar"
        ]


view : DashboardModel -> Element Msg
view model =
    row [ width fill, height fill ]
        [ viewSidebar model
        , text "Dashboard"
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
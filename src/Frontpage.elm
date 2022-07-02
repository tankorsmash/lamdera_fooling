module Frontpage exposing (update, view)

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
    Types.FrontpageModel


type alias Msg =
    Types.FrontpageMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        Types.NoOpFrontpage ->
            noop


view : Model -> Element Msg
view model =
    column [ width fill, height fill ]
        [ Element.html <| FontAwesome.Styles.css
        , row [ width fill, padding 10 ]
            [ el [ alignLeft, Font.bold, Font.color Theme.darkHeaderColor, Theme.fontFamilyPoppins ] <|
                text "Frontpage"
            , el [ alignRight ] <|
                UI.fontAwesome FAR.comment
            ]
        ]

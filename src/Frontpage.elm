module Frontpage exposing (update, view)

import AdminPage
import Angle
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
    column [ width fill, height fill, padding 100, Background.color Theme.offWhiteColor ]
        [ Element.html <| FontAwesome.Styles.css
        , -- bubble element
          column
            [ width (fill |> Element.maximum 1200)
            , height fill
            , padding 20
            , centerX
            , Border.rounded 30
            , Border.shadow
                { offset = ( 5, 16 )
                , size = 0
                , blur = 11
                , color = Theme.lightenPurpleColor 0.25
                }
            , Font.color <| Theme.offWhiteColor
            , Background.gradient
                { angle = Angle.turns 0.25 |> Angle.inRadians
                , steps = [ Theme.lightenPurpleColor 0.27, Theme.lightenPurpleColor 0.3 ]
                }
            , Font.color Theme.darkHeaderColor
            ]
            [ -- header
              row [ centerX, width fill ]
                [ row
                    [ centerX
                    , Font.bold
                    , Theme.fontFamilyPoppins
                    , spacing 5
                    , Font.size 30
                    , padding 20
                    ]
                    [ el [ Font.size 25 ] <| UI.fontAwesome <| FAS.arrowPointer
                    , text "Clikr"
                    ]
                , -- links next to header
                  row [ spacing 10, Font.size 12, alignRight ]
                    [ el [ alignRight, Font.bold ] <| text "Log in"
                    , el
                        [ alignRight
                        , Border.rounded 30
                        , Border.width 2
                        , padding 10
                        , Border.color <| Theme.lightenDarkHeaderColor 0.2
                        , Font.color <| Theme.lightenDarkHeaderColor 0.2
                        , Font.bold
                        ]
                      <|
                        text "Sign up"
                    ]
                ]
            , row [ width fill, height fill ]
                [ column [ alignTop, width fill, height fill, paddingXY 100 150, spacing 10, Font.color <| Theme.darkHeaderColor ]
                    [ el [ centerY ] <| text "You are a nobody."
                    , el [ centerY ] <| text "Become some different."
                    , el [ centerY ] <| paragraph [] [ text "Become someone", el [ Font.bold ] <| text " better." ]
                    , el [ centerY ] <| paragraph [ Font.size 30, Font.bold ] [ text "Start ", el [ Font.underline ] <| text " today!" ]
                    ]
                , column
                    [ alignRight
                    , width (fillPortion 2)
                    , height fill
                    , paddingXY 50 0
                    ]
                    [ column
                        [ width fill
                        , height fill
                        , padding 50
                        , Background.image "/imgs/frontpage_shake.png"
                        , Element.htmlAttribute <| Attr.title "Designed by pch.vector / Freepik.com"
                        ]
                        []
                    ]
                ]
            ]
        ]

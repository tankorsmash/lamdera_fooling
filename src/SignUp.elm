module SignUp exposing (update, view)

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
import Urls


type alias Model =
    Types.SignUpModel


type alias Msg =
    Types.SignUpMsg


stringToMaybe : String -> Maybe String
stringToMaybe str =
    case str of
        "" ->
            Nothing

        _ ->
            Just str


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        Types.NoOpSignUp ->
            noop

        Types.SignUpUsernameChanged username ->
            ( { model | username = stringToMaybe username }, Cmd.none )

        Types.SignUpPasswordChanged password ->
            ( { model | password = stringToMaybe password }, Cmd.none )

        Types.SignUpSubmit ->
            Debug.todo "branch 'SignUpSubmit' not implemented"


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
                    , Element.paddingEach { top = 30, left = 20, right = 20, bottom = 0 }
                    ]
                    [ el [ Font.size 25 ] <| UI.fontAwesome <| FAS.arrowPointer
                    , text "Signup"
                    ]
                , -- links next to header
                  row [ alignRight, Element.spacing 50, padding 10 ]
                    [ row [ spacing 10, Font.size 12, alignRight ]
                        [ Element.link [ alignRight, Font.size 10 ] { url = Urls.login, label = text "Returning user?" }
                        , Element.link
                            [ alignRight
                            , Border.rounded 30
                            , Border.width 2
                            , paddingXY 15 10
                            , Border.color <| Theme.lightenDarkHeaderColor 0.2
                            , Font.color <| Theme.lightenDarkHeaderColor 0.2
                            , Font.bold
                            ]
                            { label = text "Sign up", url = Urls.signUp }
                        ]
                    ]
                ]
            , row [ width fill, height fill ]
                [ column
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
                , column [ alignTop, width fill, height fill, paddingXY 100 150, spacing 10, Font.color <| Theme.darkHeaderColor ]
                    [ Input.username [ centerY ]
                        { onChange = Types.SignUpUsernameChanged
                        , text = model.username |> Maybe.withDefault ""
                        , placeholder = Just <| Input.placeholder [] <| text "What they will call you"
                        , label = Input.labelAbove [] <| text "Username"
                        }
                    , Input.newPassword [ centerY ]
                        { onChange = Types.SignUpPasswordChanged
                        , text = model.password |> Maybe.withDefault ""
                        , placeholder = Just <| Input.placeholder [] <| text "Unique password"
                        , label = Input.labelAbove [] <| text "Password"
                        , show = False
                        }
                    ]
                ]
            ]
        ]

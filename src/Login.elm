module Login exposing (suite, update, updateFromBackend, view)

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
import Expect exposing (Expectation)
import External.Animator.Animator as Animator
import FontAwesome as FA
import FontAwesome.Attributes as FAA
import FontAwesome.Layering
import FontAwesome.Regular as FAR
import FontAwesome.Solid as FAS
import FontAwesome.Styles
import Fuzz exposing (Fuzzer, int, list, string, tuple)
import Html exposing (div, span)
import Html.Attributes as Attr
import Html.Events
import Interface as UI
import Lamdera
import List.Extra
import Password
import PlayerDashboard exposing (button)
import Process
import Random
import Random.Char
import Random.String
import Shrink
import String.Extra
import Task
import Test exposing (describe, fuzz, only, test)
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
import Url.Builder as UrlBuilder
import Url.Parser as Parser exposing ((</>), Parser)
import Urls
import ZxcvbnPlus


type alias Model =
    Types.LoginModel


type alias Msg =
    Types.LoginMsg


stringToMaybe : String -> Maybe String
stringToMaybe str =
    case str of
        "" ->
            Nothing

        _ ->
            Just str


passwordMinLength : number
passwordMinLength =
    5


passwordMaxLength : number
passwordMaxLength =
    200


type RawPassword
    = RawPassword String


type ValidationErrorMessage
    = ValidationErrorMessage String


validatePasswordMinLength : RawPassword -> Result ValidationErrorMessage RawPassword
validatePasswordMinLength ((RawPassword rawPassword) as wholePassword) =
    if String.length rawPassword >= passwordMinLength then
        Ok wholePassword

    else
        Err <| ValidationErrorMessage "password is too short"


{-| Doesn't support unicode i think, because the length is an issue |
-}
validatePassword : String -> Result ValidationErrorMessage RawPassword
validatePassword password =
    password
        |> --trim whitespace
           String.trim
        |> RawPassword
        |> validatePasswordMinLength
        |> Result.andThen validatePasswordMaxLength


validatePasswordMaxLength : RawPassword -> Result ValidationErrorMessage RawPassword
validatePasswordMaxLength ((RawPassword rawPassword) as wholePassword) =
    if String.length rawPassword <= passwordMaxLength then
        Ok wholePassword

    else
        Err <| ValidationErrorMessage "password is too short"


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        Types.NoOpLogin ->
            noop

        Types.LoginUsernameChanged username ->
            ( { model
                | username = stringToMaybe username
                , --clear error on username change
                  signupSubmitError = Nothing
              }
            , Cmd.none
            )

        Types.LoginPasswordChanged password ->
            let
                isValid =
                    validatePassword password

                convertedPassword =
                    case password of
                        "" ->
                            Nothing

                        actualPassword ->
                            Just
                                { rawPassword = actualPassword
                                , passwordStrength =
                                    actualPassword
                                        |> ZxcvbnPlus.zxcvbnPlus []
                                        |> .score
                                        |> scoreToInt
                                }
            in
            ( { model
                | password = convertedPassword
                , --clear error on password change
                  signupSubmitError = Nothing
              }
            , Cmd.none
            )

        Types.LoginSubmit ->
            Maybe.map2
                (\username { rawPassword } ->
                    let
                        ( hashedPasswordResult, newSeed ) =
                            Password.generateHash rawPassword model.globalSeed
                    in
                    case hashedPasswordResult of
                        Ok hashedPassword ->
                            ( { model | globalSeed = newSeed }
                            , Types.LoginExistingUserToBackend
                                { username = username
                                , hashedPassword = hashedPassword
                                }
                                |> Types.LoginSendingToBackend
                                |> Lamdera.sendToBackend
                            )

                        Err _ ->
                            ( { model | password = Nothing, globalSeed = newSeed }, Cmd.none )
                )
                model.username
                model.password
                |> Maybe.withDefault noop


noArgsLazy : Element msg -> Element msg
noArgsLazy element =
    Lazy.lazy (always element) ()


viewHeader : Element Msg
viewHeader =
    row
        [ centerX
        , Font.bold
        , Theme.fontFamilyPoppins
        , spacing 5
        , Font.size 30
        , Element.paddingEach { top = 30, left = 20, right = 20, bottom = 0 }
        ]
        [ el [ Font.size 25 ] <| UI.fontAwesome <| FAS.arrowPointer
        , text "Login"
        ]


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
              row [ centerX, width fill ] <|
                [ noArgsLazy viewHeader
                , -- links next to header
                  row [ alignRight, Element.spacing 50, padding 10 ]
                    [ row [ spacing 10, Font.size 12, alignRight ]
                        [ Element.link
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
                        , Background.image "/imgs/signup_shake.png"
                        , Element.htmlAttribute <| Attr.title "Designed by vectorjuice / Freepik.com"
                        ]
                        []
                    ]
                , column [ alignTop, width fill, height fill, paddingXY 100 100, spacing 10, Font.color <| Theme.darkHeaderColor ] <|
                    let
                        isReadyToSubmit =
                            case ( model.username, model.password ) of
                                ( Just _, Just _ ) ->
                                    True

                                _ ->
                                    False
                    in
                    [ Input.username [ centerY ]
                        { onChange = Types.LoginUsernameChanged
                        , text = model.username |> Maybe.withDefault ""
                        , placeholder = Just <| Input.placeholder [] <| text "Who are you again?"
                        , label = Input.labelAbove [] <| text "Username"
                        }
                    , --signup error
                      model.signupSubmitError
                        |> Maybe.map
                            (el [ Font.color <| UI.color_danger ] << text)
                        |> Maybe.withDefault (text " ")
                    , --password input
                      Input.newPassword [ centerY ]
                        { onChange = Types.LoginPasswordChanged
                        , text = model.password |> Maybe.map .rawPassword |> Maybe.withDefault ""
                        , placeholder = Just <| Input.placeholder [] <| text "Your password"
                        , label = Input.labelAbove [] <| text "Password"
                        , show = False
                        }
                    , el [ Element.transparent <| not isReadyToSubmit, width fill, paddingXY 0 15 ] <|
                        button [ centerY, Border.rounded 5, Element.transparent <| model.signupSubmitError /= Nothing ]
                            Types.LoginSubmit
                            "Submit"
                    ]
                ]
            ]
        ]


viewPasswordStrength : Bool -> Int -> Element Msg
viewPasswordStrength hidePasswordStrength passwordStrength =
    let
        msg =
            "Strength: " ++ String.fromInt passwordStrength ++ "/5"

        fontColor =
            if passwordStrength <= 1 then
                Color.lightRed

            else if passwordStrength == 2 then
                Color.lightYellow |> Color.Manipulate.darken 0.25

            else
                Color.lightGreen
    in
    el
        [ Font.color <| UI.convertColor fontColor
        , Font.size 14
        , centerX
        , Element.transparent hidePasswordStrength
        ]
    <|
        text msg


scoreToInt : ZxcvbnPlus.Score -> Int
scoreToInt score =
    case score of
        ZxcvbnPlus.TooGuessable ->
            0

        ZxcvbnPlus.VeryGuessable ->
            1

        ZxcvbnPlus.SomewhatGuessable ->
            2

        ZxcvbnPlus.SafelyUnguessable ->
            3

        ZxcvbnPlus.VeryUnguessable ->
            4


updateFromBackend : Types.ToLoginFrontend -> Model -> ( Model, Cmd Msg )
updateFromBackend toLogin model =
    let
        noop =
            ( model, Cmd.none )
    in
    case toLogin of
        Types.NoOpToLoginFrontend ->
            noop

        Types.LoginRejectedUserDoesNotExist ->
            ( { model | signupSubmitError = Just "Username does not exist" }, Cmd.none )

        Types.LoginRejectedPasswordMismatch ->
            ( { model | signupSubmitError = Just "Password incorrect" }, Cmd.none )

        Types.LoginAccepted fullUser ->
            -- handled in Frontend.elm, so we can safely ignore it (in fact it should never arrive here)
            noop


stringLenFuzzer : Int -> Int -> Fuzzer String
stringLenFuzzer minLength maxLength =
    Fuzz.custom
        (Random.String.rangeLengthString
            minLength
            maxLength
            Random.Char.latin
        )
        Shrink.string


suite : Test.Test
suite =
    let
        _ =
            123
    in
    describe "password validation"
        [ fuzz (stringLenFuzzer 0 (passwordMinLength - 1)) "min length 5 is required" <|
            \rawPassword ->
                Expect.err <| validatePassword rawPassword
        , fuzz (stringLenFuzzer passwordMinLength passwordMaxLength) "length is just right" <|
            \rawPassword ->
                Expect.ok <| validatePassword rawPassword
        , fuzz (stringLenFuzzer (passwordMaxLength + 1) (passwordMaxLength + 1000)) "max length" <|
            \rawPassword ->
                Expect.err <| validatePassword rawPassword
        ]

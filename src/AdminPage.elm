module AdminPage exposing (..)

import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, explain, fill, fillPortion, height, modular, padding, paddingXY, paragraph, rgb, rgb255, row, scrollbars, spacing, spacingXY, text, width)
import Interface as UI
import Lamdera
import Types exposing (..)


type alias Model =
    AdminFrontendModel


type alias Msg =
    AdminFrontendMsg


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        NoOpAdminFrontend ->
            noop

        DownloadUsers ->
            ( model, adminSendToBackend AdminWantsToDownloadUsers )


updateFromBackend : ToAdminFrontend -> Model -> ( Model, Cmd Msg )
updateFromBackend msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        NoOpToAdminFrontend ->
            noop

        DownloadedUsers ->
            noop


view : Element Msg
view =
    column [ width fill ] <|
        [ text "New Admin Page"
        , UI.primary_button
            { buttonType = UI.Primary
            , colorTheme = UI.BrightTheme
            , customAttrs = []
            , textLabel = "Click me"
            , onPressMsg = DownloadUsers
            }
        ]

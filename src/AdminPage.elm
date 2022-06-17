module AdminPage exposing (..)

import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, explain, fill, fillPortion, height, modular, padding, paddingXY, paragraph, rgb, rgb255, row, scrollbars, spacing, spacingXY, text, width)
import Element.Font as Font
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

        DownloadAllChatMessages ->
            ( model, adminSendToBackend AdminWantsToDownloadChatMessages )


updateFromBackend : ToAdminFrontend -> Model -> ( Model, Cmd Msg )
updateFromBackend msg model =
    let
        noop =
            ( model, Cmd.none )
    in
    case msg of
        NoOpToAdminFrontend ->
            noop

        DownloadedUsers users ->
            ( { model | users = users }, Cmd.none )

        DownloadedChatMessages chatMessages ->
            ( { model | allChatMessages = chatMessages }, Cmd.none )


{-| Primary button
-}
button : Msg -> String -> Element Msg
button msg str =
    UI.primary_button
        { buttonType = UI.Primary
        , colorTheme = UI.BrightTheme
        , customAttrs = []
        , textLabel = str
        , onPressMsg = msg
        }


view : Model -> Element Msg
view model =
    let
        hasUsers =
            not <| List.isEmpty model.users
    in
    column [ width fill, spacing 5 ] <|
        [ text "New Admin Page"
        , button DownloadUsers
            (if not hasUsers then
                "Get All Users"

             else
                "Refresh Users"
            )
        , column [ spacing 5 ] <|
            if hasUsers then
                [ el [ Font.underline ] <| text "Users"
                , column []
                    (model.users
                        |> List.map getUserData
                        |> List.filterMap identity
                        |> List.map (\ud -> text ud.username)
                    )
                ]

            else
                []
        , button DownloadAllChatMessages "Download Chat Messages"
        , column [ spacing 5 ] <|
            if hasUsers then
                [ el [ Font.underline ] <| text "Chat Messages"
                , column []
                    (model.allChatMessages
                        |> List.map
                            (\chatMessage ->
                                text chatMessage.message
                            )
                    )
                ]

            else
                []
        ]

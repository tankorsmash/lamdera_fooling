module AdminPage exposing (..)

import Browser exposing (UrlRequest(..))
import Element exposing (Color, Element, alignBottom, alignLeft, alignRight, alignTop, centerX, centerY, column, el, explain, fill, fillPortion, height, modular, padding, paddingXY, paragraph, rgb, rgb255, row, scrollbars, spacing, spacingXY, text, width)
import Element.Events as Events
import Element.Font as Font
import Interface as UI
import Lamdera
import List.Extra
import Types exposing (..)
import UUID
import Url


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

        AddDummyUsers numUsers ->
            ( model, adminSendToBackend <| AdminWantsToAddDummyUsers numUsers )

        AddDummyChatMessages numChatMessages ->
            ( model, adminSendToBackend <| AdminWantsToAddDummyChatMessages numChatMessages )

        SelectChatMessage chatMessageId ->
            ( { model | selectedChatMessageUuid = chatMessageId }, Cmd.none )

        DeleteSelectedMessage messageId ->
            ( { model | selectedChatMessageUuid = Nothing }
            , adminSendToBackend <| AdminWantsToDeleteChatMessage messageId
            )


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


link : String -> String -> Element Msg
link url str =
    UI.button
        (UI.LinkTextParams
            { buttonType = UI.Outline
            , colorTheme = UI.BrightTheme
            , customAttrs = []
            , textLabel = "Back"
            , url = "/"
            }
        )


view : Model -> Element Msg
view ({ users, allChatMessages, selectedChatMessageUuid } as model) =
    let
        hasUsers =
            not <| List.isEmpty model.users

        hasChatMessages =
            not <| List.isEmpty model.allChatMessages

        isMessageSelected chatMessage =
            selectedChatMessageUuid
                |> Maybe.map
                    (\msgId ->
                        msgId == chatMessage.uuid
                    )
                |> Maybe.withDefault False

        viewChatMessage chatMessage =
            let
                clickChatMsg =
                    if isMessageSelected chatMessage then
                        SelectChatMessage Nothing

                    else
                        SelectChatMessage (Just chatMessage.uuid)
            in
            paragraph
                [ Element.pointer
                , Element.mouseOver [ Font.color <| UI.color_grey ]
                , Events.onClick <| clickChatMsg
                , if isMessageSelected chatMessage then
                    Font.underline

                  else
                    UI.noopAttr
                ]
                [ el [ UI.scaled_font 1 ] <|
                    text chatMessage.userData.username
                , text " "
                , el [ UI.scaled_font 1, UI.font_grey ] <|
                    text <|
                        "["
                            ++ (UUID.toString chatMessage.uuid |> String.toList |> List.take 5 |> String.fromList)
                            ++ "]"
                , text " "
                , text chatMessage.message
                ]
    in
    column [ width fill, spacing 5, padding 20, height fill ] <|
        [ text "New Admin Page"
        , link "/" "Back"
        , row [ width fill, spacing 10 ]
            [ button DownloadUsers
                (if not hasUsers then
                    "Get All Users"

                 else
                    "Refresh Users"
                )
            , button (AddDummyUsers 5) "Add Dummy Users x5"
            ]
        , column [ spacing 5, height (fill |> Element.minimum 100), width fill ] <|
            if hasUsers then
                [ el [ Font.underline ] <| text "Users"
                , column
                    [ UI.allowUserSelect
                    , height fill
                    , width fill
                    , Element.scrollbarY
                    ]
                    (model.users
                        |> List.map getUserData
                        |> List.filterMap identity
                        |> List.map (\ud -> text ud.username)
                    )
                ]

            else
                []
        , row [ width fill, spacing 10 ]
            [ button DownloadAllChatMessages "Download Chat Messages"
            , button (AddDummyChatMessages 5) "Add Dummy Messages x5"
            , selectedChatMessageUuid
                |> Maybe.andThen
                    (\uuid -> List.Extra.find (.uuid >> (==) uuid) allChatMessages)
                |> Maybe.map
                    (\chatMessage ->
                        column [ spacing 10 ]
                            [ Element.textColumn [ alignTop ]
                                [ paragraph [] [ text <| "Message selected: " ]
                                , paragraph [] [ text chatMessage.message ]
                                ]
                            , button (DeleteSelectedMessage chatMessage.uuid) "Delete Message"
                            ]
                    )
                |> Maybe.withDefault Element.none
            ]
        , column [ spacing 5, height (fill |> Element.minimum 100), width fill ] <|
            if hasChatMessages then
                [ el [ Font.underline ] <| text "Chat Messages"
                , column [ UI.allowUserSelect, height fill, width fill, Element.scrollbarY ]
                    (model.allChatMessages
                        |> List.map viewChatMessage
                    )
                ]

            else
                []
        ]

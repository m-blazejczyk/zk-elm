module UiEditing exposing
    ( EditorConfig
    , EditorView
    , textEditorView
    , viewEditableField
    , viewEditableImage
    )

import Paths
import Image
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui exposing (..)


-- type alias BasicEditConfig a m =
--     { a | mHint : Maybe String
--         , maxLen : Int
--         , submitMsg : m
--         , changeMsg : String -> m
--         , cancelMsg : m
--         , closeUploadErrorMsg : m
--         , startEditingImageMsg : m }


type alias EditorConfig m =
    { mHint : Maybe String
    , maxLen : Int
    , submitMsg : m
    , changeMsg : String -> m
    , cancelMsg : m
    , closeUploadErrorMsg : m
    }

type alias EditorView m = Bool -> String -> EditorConfig m -> Html m


textEditorView : Bool -> String -> EditorConfig m -> Html m
textEditorView isError val { maxLen, changeMsg } =
    let
        viewRawInput =
            input [ maxlength maxLen
                  , value val
                  , type_ "text"
                  , class "form-control"
                  , id "inPlaceEditor"
                  , onInput changeMsg ]
                []
            
    in
            
    if isError then
        div [ class "form-group has-error has-feedback full-width-input" ]
            [ viewRawInput
            , span [ class "glyphicon glyphicon-exclamation-sign form-control-feedback" ] []
            ]
    else
        div [ class "form-group full-width-input" ]
            [ viewRawInput ]


uploadEditorView : Bool -> String -> EditorConfig m -> Html m
uploadEditorView isError _ { changeMsg } =
    let
        viewRawFile =
            input [ type_ "file"
                  , class "form-control"
                  , id "inPlaceEditor"
                  , onInput changeMsg ]
                []
            
    in
            
    if isError then
        div [ class "form-group has-error has-feedback full-width-input" ]
            [ viewRawFile
            , span [ class "glyphicon glyphicon-exclamation-sign form-control-feedback" ] []
            ]
    else
        div [ class "form-group full-width-input" ]
            [ viewRawFile ]


viewInputButtons : EditorConfig m -> Html m
viewInputButtons { submitMsg, cancelMsg } =
    div [ class "btn-group right-align" ]
        [ button [ class "btn btn-default btn-sm", style "color" "green", onClick submitMsg ]
            [ glyphicon "ok" NoSpace ]
        , button [ class "btn btn-default btn-sm", style "color" "red", onClick cancelMsg ]
            [ glyphicon "remove" NoSpace ]
        ]


viewInputWrapper : EditorConfig m -> Html m -> Html m
viewInputWrapper config content =
    let
        hintHtml =
            case config.mHint of
                Just actualHint ->
                    span [ class "tytul" ] [ text <| "(" ++ actualHint ++ ")" ]

                Nothing ->
                    text ""
    in
    div [ class "full-width" ]
        [ content, hintHtml, viewInputButtons config ]


viewUploadStatus : Image.UploadStatus -> m -> Html m
viewUploadStatus uploadStatus closeUploadErrorMsg =
    case uploadStatus of
        Image.Uploading progress ->
            if progress == 100 then
                text <| "Przetwarzanie na serwerze…"
            else
                div []
                    [ text <| "Nagrywanie pliku… "
                    , div [ class "progress" ]
                        [ div [ class "progress-bar", style "width" (String.fromInt progress ++ "%") ]
                            [ span [ class "sr-only" ] [] ] ] ]

        Image.UploadFinished (Ok image) ->
            text <| "Plik nagrany: " ++ image.file

        Image.UploadFinished (Err error) ->
            viewErrorMsg (Just error) closeUploadErrorMsg


viewEditableField : Maybe e
    -> (e -> Bool)
    -> (e -> Maybe Image.UploadStatus)
    -> (e -> Bool)
    -> (e -> String)
    -> Html m
    -> EditorView m
    -> EditorConfig m
    -> Html m
viewEditableField mEditing isEdited getUploadStatus isError getValue nonEditingView editorView config =
    case mEditing of
        Just editing ->
            if isEdited editing then
                case getUploadStatus editing of
                    Just uploadStatus ->
                        viewUploadStatus uploadStatus config.closeUploadErrorMsg

                    Nothing ->
                        viewInputWrapper
                            config
                            (editorView (isError editing) (getValue editing) config)

            else
                nonEditingView

        Nothing ->
            nonEditingView

viewEditableImage : Maybe e
    -> (e -> Bool)
    -> (e -> Maybe Image.UploadStatus)
    -> (e -> Bool)
    -> (e -> String)
    -> Maybe Image.Image
    -> EditorConfig m
    -> m
    -> Html m
viewEditableImage mEditing isEdited getUploadStatus isError getValue mImage config startEditingMsg =
    let
        viewJustImage =
            case mImage of
                Just image ->
                    div [ style "text-align" "center" ]
                        [ img [ src <| Paths.images image.file, width image.width, height image.height ] []
                        , br [] []
                        , span [] [ text (String.fromInt image.width ++ " × " ++ String.fromInt image.height ++ " px") ]
                        ]

                Nothing ->
                    text "Brak obrazka"

        editingText =
            case mImage of
                Just _ ->
                    "Zmień obrazek"

                Nothing ->
                    "Dodaj obrazek"

        nonEditingView =
            div []
                [ p []
                    [ viewJustImage ]
                , p []
                    [ button [ class "btn btn-primary btn-sm"
                             , onClick <| startEditingMsg
                             ]
                             [ text editingText ] 
                    ]
                ]

    in
    viewEditableField
        mEditing
        isEdited
        getUploadStatus
        isError
        getValue
        nonEditingView
        uploadEditorView
        config

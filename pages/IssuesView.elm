module IssuesView exposing (view)

import Issues exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui exposing (..)
import Dict


viewLangVerLabel : IssueLang -> String -> Html Msg
viewLangVerLabel issueLang str =
    span [ class ("label label-" ++ (if issueLang.isPublished then "success" else "danger")
            ++ " label-availability") ]
        [ text str ]


viewAvailabilityLabel : Availability -> Html Msg
viewAvailabilityLabel avail =
    let
        getClass =
            case avail of
                InPreparation -> "default"
                Available -> "success"
                ReprintAvailable -> "success"
                OutOfPrint -> "danger"

        getText =
            case avail of
                InPreparation -> "Numer w przygotowaniu"
                Available -> "Numer dostępny"
                ReprintAvailable -> "Dodruk dostępny"
                OutOfPrint -> "Nakład wyczerpany"
    in
    
    span [ class ("label label-" ++ getClass ++ " label-availability") ]
        [ text getText ]


viewIssue : Issue -> Html Msg
viewIssue issue =
    let
        viewIssueTitle =
            "Numer " ++ String.fromInt issue.id ++ ": " ++ issueTopic issue.pl
    in
    
    div [ class "panel panel-default" ]
        [ div [ class "panel-body" ]
            [ img [ class "issue-cover", src "dummy.jpg", width 100, height 100 ] []
            , p [ class "section" ]
                [ text viewIssueTitle ]
            , p []
                [ viewLangVerLabel issue.pl "Polska wersja strony"
                , viewLangVerLabel issue.en "Angielska wersja strony"
                , viewAvailabilityLabel issue.availability
                , button [ class "btn btn-primary btn-float-right", onClick <| StartEditing issue.id ]
                    [ text "Szczegóły / Edycja" ]
                ]
            ]
        ]


editGeneralInfo : IssueEditable -> List (Html Msg)
editGeneralInfo issue =
    [ div [ class "form-group" ]
        [ label [ for "nr" ] [ text "Numer:" ]
        , input [ type_ "text", class "form-control", id "nr", value issue.id ] []
        ]
    , div [ class "form-group" ]
        [ label [ for "price" ] [ text "Cena (np. \"19 zł\"):" ]
        , input [ type_ "text", class "form-control", id "price", value issue.price ] []
        ]
    , div [ class "form-group" ]
        [ label [] [ text "Dostępność:" ]
        , div []
            [ label [ class "radio-inline" ] [ input [ type_ "radio", name "availability", checked True ] [], text "Option 1" ]
            , label [ class "radio-inline" ] [ input [ type_ "radio", name "availability", checked False ] [], text "Option 2" ]
            ]
        ]
    ]


editLang : IssueLangEditable -> List (Html Msg)
editLang lang =
    [ text "…" ]


editPanel : String -> List (Html Msg) -> Html Msg
editPanel t components =
    div [ class "panel panel-default" ]
        [ div [ class "panel-heading" ] [ h3 [ class "panel-title" ] [ text t ] ]
        , div [ class "panel-body" ] components
        ]


editIssue : IssueEditable -> Html Msg
editIssue issue =
    let
        editIssueTitle =
            if issue.isNew then
                "Nowy numer"
            else
                "Numer " ++ issue.id ++ ": " ++ issue.pl.topic
    in
    
    div [ class "modal-zk" ] [
        div [ class "modal-content-zk" ]
            [ p [ class "section" ]
                [ text editIssueTitle
                , span [ class "close-zk", onClick <| StopEditing ] [ text "✕" ]
                ]
            , editPanel "Informacje ogólne" (editGeneralInfo issue)
            , editPanel "Wersja polska" (editLang issue.pl)
            , editPanel "Wersja angielska" (editLang issue.en)
            ]
    ]

view : Model -> Html Msg
view model =
    div []
        [ viewErrorMsg model.errorMsg CloseErrorMsg
        , viewSpinner model.isLoading
        , p []
            [ button [ class "btn btn-primary", onClick AddIssueClick ]
                [ text "Dodaj numer" ]
            , button [ class "btn btn-primary", style "margin-left" "20px", onClick LoadIssuesClick ]
                [ glyphicon "refresh" NoSpace ]
            ]
        , if Dict.isEmpty model.issues then
            text ""
          else
          -- Dict.values returns the values "in the order of their keys" (see Elm's documentation).
            div [] (List.map viewIssue (List.reverse (Dict.values model.issues)))
        , Maybe.withDefault (text "") (Maybe.map editIssue model.mEditing)
        ]

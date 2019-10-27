module IssuesView exposing (view)

import Issues exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui exposing (..)


viewLangVerLabel : IssueLang -> String -> Html Msg
viewLangVerLabel issueLang str =
    span [ class ("label label-" ++ (if issueLang.isPublished then "success" else "danger"))
            , style "margin-right" "10px" ]
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
    
    span [ class ("label label-" ++ getClass)
            , style "margin-right" "10px" ]
        [ text getText ]


viewSingleIssue : Issue -> Html Msg
viewSingleIssue issue =
    div [ class "panel panel-default" ]
        [ div [ class "panel-body" ]
            [ img [ src "dummy.jpg", width 100, height 100, style "float" "left", style "margin-right" "20px" ] []
            , p [ class "section" ]
                [ text <| "Numer " ++ String.fromInt issue.id ++ ": " ++ issueTopic issue.pl ]
            , p []
                [ viewLangVerLabel issue.pl "Polska wersja strony"
                , viewLangVerLabel issue.en "Angielska wersja strony"
                , viewAvailabilityLabel issue.availability
                , button [ class "btn btn-primary", style "margin-left" "20px", style "float" "right" ]
                    [ text "Szczegóły / Edycja" ]
                ]
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
        , if List.isEmpty model.issues then
            text ""

          else
            div [] (List.map viewSingleIssue model.issues)
        ]

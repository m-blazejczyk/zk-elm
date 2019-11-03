module IssuesView exposing (view)

import Issues exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui exposing (..)


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


viewStaticIssue : Issue -> List (Html Msg)
viewStaticIssue issue =
    [ img [ class "issue-cover", src "dummy.jpg", width 100, height 100 ] []
    , p [ class "section" ]
        [ text <| "Numer " ++ String.fromInt issue.id ++ ": " ++ issueTopic issue.pl ]
    , p []
        [ viewLangVerLabel issue.pl "Polska wersja strony"
        , viewLangVerLabel issue.en "Angielska wersja strony"
        , viewAvailabilityLabel issue.availability
        , button [ class "btn btn-primary btn-float-right", onClick <| StartEditing issue.id ]
            [ text "Szczegóły / Edycja" ]
        ]
    ]


viewEditableIssue : Issue -> List (Html Msg)
viewEditableIssue issue =
    [ p [ class "issue-header" ]
        [ span [ class "section" ]
            [ text <| "Numer " ++ String.fromInt issue.id ++ ": Szczegóły / Edycja" ]
        , button [ class "btn btn-danger btn-float-right" ]
            [ text "Zamknij / Anuluj" ]
        ]
    , div [ class "container full-width" ]
        [ div [ class "row" ]
            [ div [ class "col-md-2" ] 
                [ p [ class "strong centered" ] [ text "Ogólne" ]
                ]
            , div [ class "col-md-5" ]
                [ p [ class "strong centered" ] [ text "Wersja polska" ]
                ]
            , div [ class "col-md-5" ]
                [ p [ class "strong centered" ] [ text "Wersja angielska" ]
                ]
            ]
        ]
    ]


viewIssue : Maybe Editing -> Issue -> Html Msg
viewIssue mEditing issue =
    let
        idToCompare = Maybe.withDefault -1 (Maybe.map (\m -> m.id) mEditing)
    in

    div [ class "panel panel-default" ]
        [ div [ class "panel-body" ]
            (if issue.id == idToCompare then
                viewEditableIssue issue
            else
                viewStaticIssue issue)
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
            div [] (List.map (viewIssue model.editing) model.issues)
        ]

module IssuesView exposing (view)

import Issues exposing (..)
import Global exposing (..)
import Paths
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui exposing (..)


viewSingleIssue : Issue -> Html Msg
viewSingleIssue issue =
    tr []
        [ td [] [ text (String.fromInt issue.id) ]
        ]


view : Model -> Html Msg
view model =
    let
      
        displayTable =
            table [ class "table table-bordered" ]
                [ tbody [] (List.map viewSingleIssue model.issues)
                ]

    in        
    div []
        [ viewErrorMsg model.errorMsg CloseErrorMsg
        , viewSpinner model.isLoading
        , button [ class "btn btn-primary", onClick AddIssueClick ]
            [ text "Dodaj numer" ]
        , button [ class "btn btn-primary", style "margin-left" "20px", onClick LoadIssuesClick ]
            [ glyphicon "refresh" NoSpace ]
        , if List.isEmpty model.issues then
            text ""

          else
            displayTable
        ]

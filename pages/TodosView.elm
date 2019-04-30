module TodosView exposing (view)

import Todos exposing (..)
import Global exposing (..)
import Paths
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui exposing (..)


-- text "Tu będą przyciski… Archiwizuj Usuń Dodaj zadanie"

viewTodoGroup : TodoGroup -> Html Msg
viewTodoGroup group =
    div [ class "panel panel-primary" ] 
        [ div [ class "panel-heading clearfix" ] 
            [ h3 [ class "panel-title" ]
                [ text group.name
                , button [ class "btn btn-small btn-default pull-right" ] [ text "Archiwizuj Usuń" ]
                ]
            ]
        , div [ class "panel-body" ]
            [ button [ class "btn btn-primary" ]--, onClick AddTodoGroupClick ]
                [ text "Dodaj zadanie" ]
            ]
        ]


view : Model -> Html Msg
view model =      
    div []
        [ hr [] []
        , h2 [ id "title" ] [ text "Zadania" ]
        , viewErrorMsg model.errorMsg CloseErrorMsg
        , viewSpinner model.isLoading
        , button [ class "btn btn-primary", onClick AddTodoGroupClick ]
            [ text "Dodaj grupę zadań" ]
        , button [ class "btn btn-primary", style "margin-left" "20px", onClick LoadTodosClick ]
            [ glyphicon "refresh" NoSpace ]
        , if List.isEmpty model.todos then
            em [] [ text "Brak zadań!" ]
          else
            div [ style "margin-top" "50px" ] (List.map viewTodoGroup model.todos)
        ]

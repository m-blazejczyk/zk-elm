module TodosView exposing (view)

import Todos exposing (..)
import Global exposing (..)
import Paths
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui exposing (..)


viewTodoGroup : TodoGroup -> Html Msg
viewTodoGroup group =
    text (String.fromInt group.id)


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
            div [] (List.map viewTodoGroup model.todos)
        ]

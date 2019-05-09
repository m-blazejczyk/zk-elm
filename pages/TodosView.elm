module TodosView exposing (view)

import Todos exposing (..)
import Global exposing (..)
import Paths
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui exposing (..)


statusToStyle : Status -> Attribute Msg
statusToStyle style =
    case style of
        NotStarted ->
            class "todo-status-not-started"

        Started ->
            class "todo-status-started"

        Finished ->
            class "todo-status-finished"

        Canceled ->
            class "todo-status-canceled"


priorityToBkgd : Priority -> Attribute Msg
priorityToBkgd prio =
    case prio of
        DayOrTwo->
            class "bg-danger"

        OneWeek->
            class "todo-bg-warning"

        TwoOrThreeWeeks->
            class "bg-info"

        Eventually->
            class ""


viewTodoItem : TodoItem -> Html Msg
viewTodoItem item =
    li [ class "list-group-item clearfix", priorityToBkgd item.priority ]
        [ div []
            [ div [ class "todo-status", statusToStyle item.status ]
                [ glyphicon "pencil" SpaceRight
                , text item.name
                ]
            , br [] []
            , div [ class "pull-right" ] [ text "Przyciski…" ]
            ]
        ]


viewTodoGroup : TodoGroup -> Html Msg
viewTodoGroup group =
    div [ class "panel panel-primary" ] 
        [ div [ class "panel-heading clearfix" ] 
            [ h3 [ class "panel-title" ]
                [ span [ style "font-size" "x-large" ]
                    [ glyphicon "pencil" SpaceRight
                    , text group.name
                    ]
                , span [ class "pull-right" ]
                    [ button [ class "btn btn-small btn-default" ]
                        [ glyphiconWithText "resize-small" "Zwiń" ]
                    , button [ class "btn btn-small btn-default", style "margin-left" "10px" ]
                        [ glyphiconWithText "inbox" "Archiwizuj" ]
                    , button [ class "btn btn-small btn-default", style "margin-left" "10px" ]
                        [ glyphiconWithText "trash" "Usuń" ]
                    ]
                ]
            ]
        , div [ class "panel-body" ]
            [ button [ class "btn btn-primary" ]--, onClick AddTodoGroupClick ]
                [ glyphiconWithText "plus-sign" "Dodaj zadanie" ]
            ]
        , if List.isEmpty group.items then
            text ""
          else
            ul [ class "list-group" ] (List.map viewTodoItem group.items)
        ]


view : Model -> Html Msg
view model =      
    div []
        [ hr [] []
        , h2 [ id "title" ] [ text "Zadania" ]
        , viewErrorMsg model.errorMsg CloseErrorMsg
        , viewSpinner model.isLoading
        , button [ class "btn btn-primary", onClick LoadTodosClick ]
            [ glyphicon "refresh" NoSpace ]
        , button [ class "btn btn-primary", onClick AddTodoGroupClick, style "margin-left" "20px" ]
            [ glyphiconWithText "plus-sign" "Dodaj grupę zadań" ]
        , if List.isEmpty model.todos then
            h3 [] [ text "Brak zadań!" ]
          else
            div [ style "margin-top" "50px" ] (List.map viewTodoGroup model.todos)
        ]

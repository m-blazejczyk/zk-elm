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


inputButtons : Html Msg
inputButtons =
    div [ class "btn-group right-align" ]
        [ button [ class "btn btn-default btn-sm", style "color" "green", onClick SubmitEditingClick ]
            [ glyphicon "ok" NoSpace ]
        , button [ class "btn btn-default btn-sm", style "color" "red", onClick CancelEditing ]
            [ glyphicon "remove" NoSpace ]
        ]


textEditor : String -> Html Msg
textEditor val =
    div [ class "form-group full-width-input" ]
        [ input [ maxlength 50 -- Hard-coded!
                  , value val
                  , type_ "text"
                  , class "form-control"
                  , id "inPlaceEditor"
                  , onInput ChangeInput ]
                []
        ]


itemName : Maybe Editing -> TodoGroup -> TodoItem -> Html Msg
itemName mEditing group item =
    case mEditing of
        Nothing ->
            div [ class "todo-status"
                , statusToStyle item.status
                , onClick <| StartEditing (Item group.id item.id) item.name ]
                [ button [ class "btn btn-small btn-default", style "margin-right" "10px" ]
                    [ glyphicon "pencil" NoSpace ]
                , text item.name
                ]

        -- Here we need check the id so that we only display the editor for the entry
        -- that is being edited
        Just editing ->
            div [ class "todo-status full-width"
                , statusToStyle item.status ]
                [ textEditor editing.value, inputButtons ]


groupHeaderNotEditingStyleBasic : Attribute Msg
groupHeaderNotEditingStyleBasic = 
    style "font-size" "x-large"


groupHeaderNotEditingStyle : Bool -> TodoGroup -> List (Attribute Msg)
groupHeaderNotEditingStyle isClickable group =
    if isClickable then
        [ groupHeaderNotEditingStyleBasic
        , onClick <| StartEditing (Group group.id) group.name ]
    else
        [ groupHeaderNotEditingStyleBasic ]

groupHeaderNotEditing : Bool -> TodoGroup -> Html Msg
groupHeaderNotEditing isClickable group =
    h3 [ class "panel-title" ]
        [ span
            (groupHeaderNotEditingStyle isClickable group)
            [ button [ class "btn btn-small btn-default", style "margin-right" "10px" ]
                [ glyphicon "pencil" NoSpace ]
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


groupHeader : Maybe Editing -> TodoGroup -> Html Msg
groupHeader mEditing group =
    case mEditing of
        Nothing ->
            groupHeaderNotEditing True group

        Just editing ->
            h3 [ class "panel-title" ]
                [ span [ style "font-size" "x-large" ]
                    [ textEditor editing.value, inputButtons ]
                ]


viewTodoItem : Maybe Editing -> TodoGroup -> TodoItem -> Html Msg
viewTodoItem mEditing group item =
    li [ class "list-group-item clearfix", priorityToBkgd item.priority ]
        [ div []
            [ itemName mEditing group item
            , br [] []
            , div [ class "pull-right" ] [ text "Dropdowns…" ]
            ]
        ]


viewTodoGroup : Maybe Editing -> TodoGroup -> Html Msg
viewTodoGroup mEditing group =
    div [ class "panel panel-primary" ] 
        [ div [ class "panel-heading clearfix" ] 
            [ groupHeader mEditing group ]
        , div [ class "panel-body" ]
            [ button [ class "btn btn-primary" ]
                [ glyphiconWithText "plus-sign" "Dodaj zadanie" ]
            ]
        , if List.isEmpty group.items then
            text ""
          else
            ul [ class "list-group" ] (List.map (viewTodoItem mEditing group) group.items)
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
            div [ style "margin-top" "50px" ] (List.map (viewTodoGroup model.mEditing) model.todos)
        ]

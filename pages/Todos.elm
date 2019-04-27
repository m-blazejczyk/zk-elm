module Todos exposing
    ( TodoGroup
    , TodoItem
    , Model
    , Msg(..)
    , init
    , switchToPageCmd
    , update
    )


import Debug exposing (log)
import Global exposing (..)
import Http
import Json.Decode exposing (Decoder, bool, int, list, string, succeed, fail, field, decodeValue, andThen)
import Json.Decode.Pipeline exposing (required)
import Result
import Task
import Browser.Dom as Dom
import Url
import Paths


type Msg
    = LoadTodosClick
    | LoadTodos (Result Http.Error (List TodoGroup))
    | AddTodoGroupClick
    | AddTodoGroup (Result Http.Error TodoGroup)
    | CloseErrorMsg


type Priority
    = DayOrTwo
    | OneWeek
    | TwoOrThreeWeeks
    | Eventually


type Status
    = NotStarted
    | Started
    | Finished
    | Canceled


type alias TodoGroup =
    { id : Int
    , name : String
    , order : Int
    , isArchived : Bool
    , items : List TodoItem
    }


type alias TodoItem =
    { id : Int
    , name : String
    , order : Int
    , priority : Priority
    , status : Status
    }


type alias Model =
    { isLoading : Bool
    , errorMsg : Maybe String
    , todos : List TodoGroup
    }


init : Model
init =
    Model False Nothing []


endpoint : List String
endpoint = [ "todos" ]


newTodoGroup : TodoGroup
newTodoGroup =
    TodoGroup -1 "Nowa kategoria" 1 False []


newTodoItem : TodoItem
newTodoItem =
    TodoItem -1 "Nowe zadanie" 1 OneWeek NotStarted


todoGroupDecoder : Decoder TodoGroup
todoGroupDecoder = 
    succeed TodoGroup
        |> required "id" int
        |> required "name" string
        |> required "order" int
        |> required "isArchived" bool
        |> required "items" (list todoItemDecoder)


todoItemDecoder : Decoder TodoItem
todoItemDecoder = 
    succeed TodoItem
        |> required "id" int
        |> required "name" string
        |> required "order" int
        |> required "priority" priorityDecoder
        |> required "status" statusDecoder


priorityDecoder : Decoder Priority
priorityDecoder =
    int |> andThen (\prio ->
           case prio of
                0 ->
                    succeed DayOrTwo
                1 ->
                    succeed OneWeek
                2 ->
                    succeed TwoOrThreeWeeks
                3 ->
                    succeed Eventually
                somethingElse ->
                    fail <| "Unknown priority: " ++ (String.fromInt somethingElse)
        )


statusDecoder : Decoder Status
statusDecoder =
    int |> andThen (\stat ->
           case stat of
                0 ->
                    succeed NotStarted
                1 ->
                    succeed Started
                2 ->
                    succeed Finished
                3 ->
                    succeed Canceled
                somethingElse ->
                    fail <| "Unknown status: " ++ (String.fromInt somethingElse)
        )


switchToPageCmd : Cmd Msg
switchToPageCmd =
    toCmd LoadTodosClick


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model token =
    case msg of
        LoadTodosClick ->
            ( { model | todos = [], isLoading = True, errorMsg = Nothing }  --, editing = Nothing
            , Http.send LoadTodos (authGetRequestExpectJson endpoint token (list todoGroupDecoder))
            )

        LoadTodos (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| Debug.log "Error" (httpErrToString err) }
            , Cmd.none
            )

        LoadTodos (Ok todos) ->
            ( { model | todos = Debug.log "Success" todos, isLoading = False, errorMsg = Nothing }
            , Cmd.none
            )

        AddTodoGroupClick ->
            ( { model | isLoading = True, errorMsg = Nothing }  --, editing = Nothing
            , Cmd.none --Http.send AddTodoGroup (authPostRequestExpectJson (endpoint ++ [ "new" ]) token issueDecoder)
            )

        AddTodoGroup (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| httpErrToString err }
            , Cmd.none
            )

        AddTodoGroup (Ok todoGroup) ->
            ( { model | todos = todoGroup :: model.todos, isLoading = False, errorMsg = Nothing }
            , Cmd.none
            )

        CloseErrorMsg ->
            ( { model | errorMsg = Nothing }, Cmd.none )

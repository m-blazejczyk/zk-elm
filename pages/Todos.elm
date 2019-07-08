module Todos exposing
    ( TodoGroup
    , TodoItem
    , Model
    , Editing
    , EditingWhat(..)
    , Msg(..)
    , Priority(..)
    , Status(..)
    , init
    , switchToPageCmd
    , update
    )


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
    | StartEditing EditingWhat String
    | ChangeInput String
    | SubmitEditingClick
    | SubmitEditing (Result Http.Error ())
    | CancelEditing
    | FocusResult (Result Dom.Error ())


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


type EditingWhat
    = Group Int
    | Item Int Int  -- Item groupId itemId


type alias Editing =
    { what : EditingWhat
    , value : String
    , isError : Bool
    }


type alias Model =
    { isLoading : Bool
    , errorMsg : Maybe String
    , todos : List TodoGroup
    , mEditing : Maybe Editing
    }


init : Model
init =
    Model False Nothing [] Nothing


groupEndpoint : List String
groupEndpoint = [ "todos", "group" ]


itemEndpoint : List String
itemEndpoint = [ "todos", "item" ]


newTodoGroup : TodoGroup
newTodoGroup =
    TodoGroup -1 "Nowa grupa zadań" 1 False []


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


inPlaceEditorId : String
inPlaceEditorId =
    "inPlaceEditor"


updateGroupName : Int -> String -> TodoGroup -> TodoGroup
updateGroupName groupId value group =
    if groupId == group.id then
        { group | name = value }
    else
        group


updateItemName : Int -> String -> TodoItem -> TodoItem
updateItemName itemId value item =
    if itemId == item.id then
        { item | name = value }
    else
        item


updateItemInGroup : Int -> Int -> String -> TodoGroup -> TodoGroup
updateItemInGroup groupId itemId value group =
    if groupId == group.id then
        { group | items = List.map (updateItemName itemId value) group.items }
    else
        group


handleSubmitEditingClick : Model -> String -> ( Model, Cmd Msg )
handleSubmitEditingClick model token =
    case model.mEditing of
        Just editing ->
            case editing.what of
                Group groupId ->
                    ( { model
                        | todos = List.map (updateGroupName groupId editing.value) model.todos
                        , mEditing = Nothing
                        , errorMsg = Nothing
                      }
                    , Http.send SubmitEditing (authPutFieldRequest groupEndpoint token groupId "name" editing.value)
                    )

                Item groupId itemId ->
                    ( { model
                        | todos = List.map (updateItemInGroup groupId itemId editing.value) model.todos
                        , mEditing = Nothing
                        , errorMsg = Nothing
                      }
                    , Http.send SubmitEditing (authPutFieldRequest itemEndpoint token itemId "name" editing.value)
                    )

        -- This should never happen!!!
        Nothing ->
            ( model, Cmd.none )


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model token =
    case msg of
        LoadTodosClick ->
            ( { model | todos = [], isLoading = True, errorMsg = Nothing, mEditing = Nothing }
            , Http.send LoadTodos (authGetRequestExpectJson groupEndpoint token (list todoGroupDecoder))
            )

        LoadTodos (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| httpErrToString err }
            , Cmd.none
            )

        LoadTodos (Ok todos) ->
            ( { model | todos = todos, isLoading = False, errorMsg = Nothing }
            , Cmd.none
            )

        AddTodoGroupClick ->
            ( { model | isLoading = True, errorMsg = Nothing, mEditing = Nothing }
            , Cmd.none --Http.send AddTodoGroup (authPostRequestExpectJson (groupEndpoint ++ [ "new" ]) token groupDecoder)
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

        StartEditing what value ->
            ( { model | mEditing = Just (Editing what value False) }
            , Dom.focus inPlaceEditorId |> Task.attempt FocusResult
            )

        ChangeInput newVal ->
            case model.mEditing of
                Just oldEditing ->
                    let
                        newEditing =
                            { oldEditing | value = newVal }
                    in
                    ( { model | mEditing = Just newEditing }, Cmd.none )

                -- This should never happen!!!
                Nothing ->
                    ( model, Cmd.none )

        SubmitEditingClick ->
            handleSubmitEditingClick model token

        SubmitEditing (Err err) ->
            ( { model | errorMsg = Just <| httpErrToString err }
            , Cmd.none
            )

        SubmitEditing (Ok ()) ->
            ( { model | errorMsg = Nothing }
            , Cmd.none
            )

        CancelEditing ->
            ( { model | mEditing = Nothing }, Cmd.none )

        FocusResult _ ->
            ( model, Cmd.none )

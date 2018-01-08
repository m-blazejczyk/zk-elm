module Banners exposing
    ( Msg(..), Banner, Editing, Validator, Model, Column(..)
    , init, update
    , validateWeight, validateUrl, validateDate, modifyWeight, modifyUrl, modifyDate
    )

import Global exposing (..)
import Dom
import Task
import Result
import Regex


type Column
    = SilentColumn
    | ImageColumn
    | StartDateColumn
    | EndDateColumn
    | UrlColumn
    | WeightColumn
    | ActionsColumn


type alias Validator = String -> Bool


type alias Modifier = String -> Banner -> Banner


type Msg
    = ChangeSilent Int Bool
    | StartEditing Int Column String
    | ChangeInput String
    | ValidateEditing Validator Modifier
    | CancelEditing
    | FocusResult (Result Dom.Error ())


type alias Banner =
    { id : Int
    , isSilent : Bool
    , startDate : Maybe SimpleDate
    , endDate : Maybe SimpleDate
    , image : String
    , imageH : Int
    , imageW : Int
    , url : String
    , weight : Int
    }


type alias Editing =
    { id : Int
    , column : Column
    , value : String
    , isError : Bool
    }


type alias Model =
    { banners : List Banner
    , editing : Maybe Editing
    }


init : Model
init =
    Model
        [ Banner 1 False Nothing (stringToDate "2018-1-15") "http://www.zeszytykomiksowe.org/aktualnosci/bannery/dydaktyczny-potencjal.jpg" 89 200 "http://fundacja-ikp.pl/wydawnictwo/" 10
        , Banner 2 True (stringToDate "2018-1-1") Nothing "http://www.zeszytykomiksowe.org/aktualnosci/bannery/dydaktyczny-potencjal.jpg" 89 200 "http://www.cbc.ca/news/canada/montreal/montreal-together-spaces-reconciliation-1.4117290" 20
        ]
        Nothing


updateSilent : Int -> Bool -> Banner -> Banner
updateSilent id checked banner =
    if id == banner.id then
        { banner | isSilent = checked }
    else
        banner


validateWeight: Validator
validateWeight strVal = 
    case String.toInt strVal of
        Ok _ ->
            True
        Err _ ->
            False


modifyWeight: Modifier
modifyWeight strVal banner =
    case String.toInt strVal of
        Ok intVal ->
            { banner | weight = intVal }
        Err _ ->
            banner


validateUrl: Validator
validateUrl _ =
    True  -- No validation of URLs


modifyUrl: Modifier
modifyUrl newUrl banner =
    { banner | url = newUrl }


validateDate: Validator
validateDate dateStr =
    if String.length dateStr == 0 then
        True
    else
        maybeIsJust <| stringToDate dateStr


modifyDate: Column -> Modifier
modifyDate column newDateStr banner =
    let
        newDate = stringToDate newDateStr

    in
        if column == StartDateColumn then
            { banner | startDate = newDate }
        else
            { banner | endDate = newDate }


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSilent id checked ->
            ( { model | banners = List.map (updateSilent id checked) model.banners }, Cmd.none )

        StartEditing id column value ->
            ( { model | editing = Just (Editing id column value False) }
            , Dom.focus "inPlaceEditor" |> Task.attempt FocusResult )

        ChangeInput newVal ->
            case model.editing of
                Just oldEditing ->
                    let
                        newEditing = { oldEditing | value = newVal }
                    in
                        ( { model | editing = Just newEditing }, Cmd.none )
                Nothing ->
                    ( model, Cmd.none )  -- This should never happen!!!

        ValidateEditing valFun modFun ->
            let
                updateField : Editing -> Banner -> Banner
                updateField editing banner =
                    if editing.id == banner.id then
                        modFun editing.value banner
                    else
                        banner

                setEditingError editing = { editing | isError = True }

            in

                case model.editing of
                    Just editing ->
                        if valFun editing.value then
                            ( { model | banners = List.map (updateField editing) model.banners
                                , editing = Nothing }
                            , Cmd.none )
                        else
                            ( { model | editing = Just <| setEditingError editing }
                            , Dom.focus "inPlaceEditor" |> Task.attempt FocusResult )

                    Nothing ->
                        ( model, Cmd.none )  -- This should never happen!!!

        CancelEditing ->
            ( { model | editing = Nothing }, Cmd.none )

        FocusResult result ->
            ( model, Cmd.none )

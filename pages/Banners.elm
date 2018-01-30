module Banners exposing
    ( Msg(..), SortOrder(..), Banner, Editing, Validator, Model, Column(..)
    , init, update, isColumnSortable
    , validateWeight, validateUrl, validateDate, modifyWeight, modifyUrl, modifyDate
    )

import Global exposing (..)
import Dict
import Dom
import Http
import Task
import Result
import Regex
import Json.Decode exposing (Decoder, list, string, int, bool, nullable)
import Json.Decode.Pipeline exposing (decode, required)


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


type SortOrder
    = Ascending
    | Descending


type Msg
    = ChangeSilent Int Bool
    | StartEditing Int Column String
    | ChangeInput String
    | ValidateEditing Validator Modifier
    | CancelEditing
    | FocusResult (Result Dom.Error ())
    | DeleteRow Int
    | AddRow
    | SwitchSort Column
    | LoadBanners
    | LoadBannersHttp (Result Http.Error (List Banner))


type alias Image =
    { file : String
    , height : Int
    , width : Int
    }


type alias Banner =
    { id : Int
    , isSilent : Bool
    , startDate : Maybe SimpleDate
    , endDate : Maybe SimpleDate
    , image : Maybe Image
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
    , sortOrder : Maybe (Column, SortOrder)
    }


init : Model
init =
    Model
        [ Banner 1 False Nothing (stringToDate "2018-1-15") (Just (Image "http://www.zeszytykomiksowe.org/aktualnosci/bannery/dydaktyczny-potencjal.jpg" 89 200)) "http://fundacja-ikp.pl/wydawnictwo/" 10
        , Banner 2 True (stringToDate "2018-1-1") Nothing (Just (Image "http://www.zeszytykomiksowe.org/aktualnosci/bannery/dydaktyczny-potencjal.jpg" 89 200)) "http://www.cbc.ca/news/canada/montreal/montreal-together-spaces-reconciliation-1.4117290" 20
        ]
        Nothing
        (Just ( WeightColumn, Ascending ))


newBanner : Banner
newBanner =
    Banner -1 False Nothing Nothing Nothing "" 10


simpleDateDecoder : Decoder (Maybe SimpleDate)
simpleDateDecoder =
    Json.Decode.map stringToDate string


imageDecoder : Decoder Image
imageDecoder = 
    decode Image
        |> required "file" string
        |> required "height" int
        |> required "width" int


bannerDecoder : Decoder Banner
bannerDecoder =
    decode Banner
        |> required "id" int
        |> required "isSilent" bool
        |> required "startDate" simpleDateDecoder
        |> required "endDate" simpleDateDecoder
        |> required "image" (nullable imageDecoder)
        |> required "url" string
        |> required "weight" int


bannerListDecoder : Decoder (List Banner)
bannerListDecoder =
    list bannerDecoder


isColumnSortable : Column -> Bool
isColumnSortable column =
    column /= ImageColumn && column /= ActionsColumn


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


sortBy : Column -> Banner -> Banner -> Order
sortBy column b1 b2 =
    case column of
        SilentColumn ->
            if b1.isSilent == b2.isSilent then
                EQ
            else if b1.isSilent then
                LT
            else
                GT

        ImageColumn ->
            EQ

        StartDateColumn ->
            compareMaybeDates b1.startDate b2.startDate

        EndDateColumn ->
            compareMaybeDates b1.endDate b2.endDate

        UrlColumn ->
            compare b1.url b2.url

        WeightColumn ->
            compare b1.weight b2.weight

        ActionsColumn ->
            EQ


switchSort : Column -> Model -> Model
switchSort column oldModel =
    let
        newBanners = 
            case oldModel.sortOrder of
                Just (oldColumn, oldSortOrder) ->
                    -- Properly handle the case when we only need to reverse the list
                    if oldColumn == column then
                        List.reverse oldModel.banners
                    else
                        List.sortWith (sortBy column) oldModel.banners

                Nothing ->
                    List.sortWith (sortBy column) oldModel.banners
        
        newSortOrder =
            case oldModel.sortOrder of
                Just (oldColumn, oldSortOrder) ->
                    -- Properly handle the case when we only need to reverse the list
                    if oldColumn == column then
                        if oldSortOrder == Ascending then
                            Just ( column, Descending )
                        else
                            Just ( column, Ascending )
                    else
                        Just ( column, Descending )

                Nothing ->
                        Just ( column, Descending )

    in

        { oldModel | banners = newBanners, sortOrder = newSortOrder }


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

        DeleteRow id ->
            ( { model | banners = List.filter ((/=) id << .id) model.banners
              , editing = Nothing }
            , Cmd.none )

        AddRow ->
            ( { model | banners = newBanner :: model.banners }, Cmd.none )

        SwitchSort column ->
            ( switchSort column model, Cmd.none )

        -- Authorization = s'TTTKKK'
        LoadBanners ->
            ( model, Http.send LoadBannersHttp <| Http.get (domain ++ "banners") bannerListDecoder )

        LoadBannersHttp (Err err) ->
            ( model, Cmd.none )

        LoadBannersHttp (Ok banners) ->
            ( { model | banners = banners, editing = Nothing, sortOrder = Nothing }, Cmd.none )

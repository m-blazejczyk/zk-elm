module Banners exposing
    ( Msg(..), SortOrder(..), Banner, Editing, Validator, Model, Column(..)
    , init, update, switchToPageCmd, isColumnSortable
    , validateWeight, validateUrl, validateDate, modifyWeight, modifyUrl, modifyDate
    )

import Global exposing (..)
import Debug exposing (log)
import Dict
import Dom
import Http
import Task
import Time exposing (Time)
import Result
import Regex
import Json.Decode exposing (Decoder, list, oneOf, string, int, bool, nullable, null)
import Json.Decode.Pipeline exposing (decode, required)


type Column
    = SilentColumn
    | ImageColumn
    | StartDateColumn
    | EndDateColumn
    | UrlColumn
    | WeightColumn
    | ActionsColumn


-- Take raw value, validate it; return Nothing in case of an error,
-- and a tuple of (field name, field value) in case of a success
type alias Validator = String -> Maybe ( String, String )


-- Take value and banner, modify the banner, and return it
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
    | SwitchSort Column
    | AddBannerClick
    | AddBanner (Result Http.Error Banner)
    | LoadBannersClick
    | LoadBanners (Result Http.Error (List Banner))
    | DeleteBannerClick Int
    | DeleteBanner Int (Result Http.Error ())
    | CloseErrorMsg


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
    { isLoading : Bool
    , errorMsg : Maybe String
    , banners : List Banner
    , editing : Maybe Editing
    , sortOrder : Maybe (Column, SortOrder)
    }


init : Model
init =
    Model False Nothing [] Nothing Nothing


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
        |> required "startDate" (oneOf [ simpleDateDecoder, null Nothing ])
        |> required "endDate" (oneOf [ simpleDateDecoder, null Nothing ])
        |> required "image" (nullable imageDecoder)
        |> required "url" string
        |> required "weight" int


switchToPageCmd : Cmd Msg
switchToPageCmd = 
    toCmd LoadBannersClick


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
        Ok v ->
            Just ( "weight", toString v )
        Err _ ->
            Nothing


modifyWeight: Modifier
modifyWeight strVal banner =
    case String.toInt strVal of
        Ok intVal ->
            { banner | weight = intVal }
        Err _ ->
            banner


validateUrl: Validator
validateUrl strVal =
    Just ( "url", strVal )  -- No validation of URLs


modifyUrl: Modifier
modifyUrl newUrl banner =
    { banner | url = newUrl }


validateDate: String -> Validator
validateDate fieldName dateStr =
    if String.length dateStr == 0 then
        Just ( fieldName, "" )
    else
        case stringToDate dateStr of
            Just date ->
                Just ( fieldName, dateToString date )

            Nothing ->
                Nothing


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
            ( { model | banners = List.map (updateSilent id checked) model.banners }
              , Cmd.none )  -- Send HTTP request here!!!

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
                updateField : Editing -> String -> Banner -> Banner
                updateField editing value banner =
                    if editing.id == banner.id then
                        modFun value banner
                    else
                        banner

                setEditingError editing = { editing | isError = True }

            in

                case model.editing of
                    Just editing ->
                        case valFun editing.value of
                            Just ( fieldName, fieldValue ) ->
                                ( { model | banners = List.map (updateField editing fieldValue) model.banners
                                    , editing = Nothing }
                                , Cmd.none )  -- Send HTTP request here!

                            Nothing ->
                                ( { model | editing = Just <| setEditingError editing }
                                , Dom.focus "inPlaceEditor" |> Task.attempt FocusResult )

                    Nothing ->
                        ( model, Cmd.none )  -- This should never happen!!!

        CancelEditing ->
            ( { model | editing = Nothing }, Cmd.none )

        FocusResult result ->
            ( model, Cmd.none )

        SwitchSort column ->
            ( switchSort column model, Cmd.none )

        AddBannerClick ->
            ( { model | errorMsg = Nothing, editing = Nothing, isLoading = True }
            , Http.send AddBanner (authPostRequestExpectJson "banners" bannerDecoder) )

        AddBanner (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| toString err }
            , Cmd.none )

        AddBanner (Ok banner) ->
            ( { model | banners = banner :: model.banners, isLoading = False, errorMsg = Nothing }
            , Cmd.none )

        LoadBannersClick ->
            ( { model | banners = [], isLoading = True, errorMsg = Nothing, editing = Nothing, sortOrder = Nothing }
            , Http.send LoadBanners (authGetRequestExpectJson "banners" (list bannerDecoder)) )

        LoadBanners (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| toString err }
            , Cmd.none )

        LoadBanners (Ok banners) ->
            ( { model | banners = banners, errorMsg = Nothing, isLoading = False }
            , Cmd.none )

        DeleteBannerClick id ->
            ( { model | errorMsg = Nothing, editing = Nothing, isLoading = True }
            , Http.send (DeleteBanner id) (authDeleteRequest "banners" id) )

        DeleteBanner _ (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| toString err }
            , Cmd.none )

        DeleteBanner id (Ok ()) ->
            ( { model | banners = List.filter ((/=) id << .id) model.banners
              , errorMsg = Nothing, isLoading = False }
            , Cmd.none )

        CloseErrorMsg ->
            ( { model | errorMsg = Nothing }, Cmd.none )

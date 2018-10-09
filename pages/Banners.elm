port module Banners exposing
    ( Banner
    , Column(..)
    , Editing
    , Model
    , Msg(..)
    , SortOrder(..)
    , Validator
    , init
    , isColumnSortable
    , modifyDate
    , modifyUrl
    , modifyWeight
    , switchToPageCmd
    , update
    , validateDate
    , validateUrl
    , validateWeight
    )

import Debug exposing (log)
import Dict
import Global exposing (..)
import Http
import Json.Decode exposing (Decoder, bool, int, list, null, nullable, oneOf, string, succeed)
import Json.Decode.Pipeline exposing (required)
import Json.Encode as E
import Regex
import Result
import Task
import Browser.Dom as Dom
import Url


type Column
    = SilentColumn
    | ImageColumn
    | StartDateColumn
    | EndDateColumn
    | UrlColumn
    | WeightColumn
    | ActionsColumn



-- Take raw value, validate it; return Nothing in case of an error,
-- and the cleaned up field value in case of a success


type alias Validator =
    String -> Maybe String



-- Take value and banner, modify the banner, and return it


type alias Modifier =
    String -> Banner -> Banner


type SortOrder
    = Ascending
    | Descending


type Msg
    = ChangeSilent Int Bool
    | StartEditing Int Column String
    | ChangeInput String
    | ValidateEditing Validator Modifier
    | SubmitFileUpload Validator Modifier
    | FileUploadStatus E.Value
    | SubmitEditing (Result Http.Error ())
    | CancelEditing
    | FocusResult (Result Dom.Error ())
    | SwitchSort Column
    | AddBannerClick
    | AddBanner (Result Http.Error SerializableBanner)
    | LoadBannersClick
    | LoadBanners (Result Http.Error (List SerializableBanner))
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
    , url : Maybe String
    , weight : Int
    }


type alias SerializableBanner =
    { id : Int
    , isSilent : Bool
    , startDate : Maybe SimpleDate
    , endDate : Maybe SimpleDate
    , imageUrl : Maybe String
    , imageHeight : Maybe Int
    , imageWidth : Maybe Int
    , url : Maybe String
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
    , sortOrder : Maybe ( Column, SortOrder )
    }


port initiateFileUpload : String -> Cmd msg


inPlaceEditorId : String
inPlaceEditorId =
    "inPlaceEditor"


init : Model
init =
    Model False Nothing [] Nothing Nothing


endpoint : List String
endpoint = [ "banners" ]


newBanner : Banner
newBanner =
    Banner -1 False Nothing Nothing Nothing Nothing 10


simpleDateDecoder : Decoder (Maybe SimpleDate)
simpleDateDecoder =
    Json.Decode.map stringToDate string


bannerDecoder : Decoder SerializableBanner
bannerDecoder =
    succeed SerializableBanner
        |> required "id" int
        |> required "isSilent" bool
        |> required "startDate" (oneOf [ simpleDateDecoder, null Nothing ])
        |> required "endDate" (oneOf [ simpleDateDecoder, null Nothing ])
        |> required "imageUrl" (nullable string)
        |> required "imageHeight" (nullable int)
        |> required "imageWidth" (nullable int)
        |> required "url" (nullable string)
        |> required "weight" int


deserialize : SerializableBanner -> Banner
deserialize sb =
    Banner
        sb.id
        sb.isSilent
        sb.startDate
        sb.endDate
        (Maybe.map3 Image sb.imageUrl sb.imageHeight sb.imageHeight)
        sb.url
        sb.weight


setEditingError : Editing -> Editing
setEditingError editing =
    { editing | isError = True }


switchToPageCmd : Cmd Msg
switchToPageCmd =
    toCmd LoadBannersClick


isColumnSortable : Column -> Bool
isColumnSortable column =
    column /= ImageColumn && column /= ActionsColumn


fieldNameFor : Column -> String
fieldNameFor column =
    case column of
        SilentColumn ->
            "silent"

        StartDateColumn ->
            "startDate"

        EndDateColumn ->
            "endDate"

        UrlColumn ->
            "url"

        WeightColumn ->
            "weight"

        _ ->
            ""


updateSilent : Int -> Bool -> Banner -> Banner
updateSilent id checked banner =
    if id == banner.id then
        { banner | isSilent = checked }

    else
        banner


validateWeight : Validator
validateWeight strVal =
    case String.toInt strVal of
        Just v ->
            Just <| String.fromInt v

        Nothing ->
            Nothing


modifyWeight : Modifier
modifyWeight strVal banner =
    case String.toInt strVal of
        Just intVal ->
            { banner | weight = intVal }

        Nothing ->
            banner


validateUrl : Validator
validateUrl strVal =
    case Url.fromString strVal of
        Nothing ->
            Nothing

        Just _ ->
            Just strVal


modifyUrl : Modifier
modifyUrl newUrl banner =
    { banner
        | url =
            if String.length newUrl == 0 then
                Nothing

            else
                Just newUrl
    }


validateDate : Validator
validateDate dateStr =
    if String.length dateStr == 0 then
        Just ""

    else
        case stringToDate dateStr of
            Just date ->
                Just <| dateToString date

            Nothing ->
                Nothing


modifyDate : Column -> Modifier
modifyDate column newDateStr banner =
    let
        newDate =
            stringToDate newDateStr
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
            compare (Maybe.withDefault "" b1.url) (Maybe.withDefault "" b2.url)

        WeightColumn ->
            compare b1.weight b2.weight

        ActionsColumn ->
            EQ


switchSort : Column -> Model -> Model
switchSort column oldModel =
    let
        newBanners =
            case oldModel.sortOrder of
                Just ( oldColumn, oldSortOrder ) ->
                    -- Properly handle the case when we only need to reverse the list
                    if oldColumn == column then
                        List.reverse oldModel.banners

                    else
                        List.sortWith (sortBy column) oldModel.banners

                Nothing ->
                    List.sortWith (sortBy column) oldModel.banners

        newSortOrder =
            case oldModel.sortOrder of
                Just ( oldColumn, oldSortOrder ) ->
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


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model token =
    case msg of
        ChangeSilent id checked ->
            ( { model | banners = List.map (updateSilent id checked) model.banners }
            , Http.send SubmitEditing (authPutFieldRequest endpoint token id "silent" (boolToString checked))
            )

        StartEditing id column value ->
            ( { model | editing = Just (Editing id column value False) }
            , Dom.focus inPlaceEditorId |> Task.attempt FocusResult
            )

        ChangeInput newVal ->
            case model.editing of
                Just oldEditing ->
                    let
                        newEditing =
                            { oldEditing | value = newVal }
                    in
                    ( { model | editing = Just newEditing }, Cmd.none )

                -- This should never happen!!!
                Nothing ->
                    ( model, Cmd.none )

        ValidateEditing valFun modFun ->
            let
                updateField : Editing -> String -> Banner -> Banner
                updateField editing value banner =
                    if editing.id == banner.id then
                        modFun value banner

                    else
                        banner

            in
            case model.editing of
                Just editing ->
                    case valFun editing.value of
                        Just fieldValue ->
                            ( { model
                                | banners = List.map (updateField editing fieldValue) model.banners
                                , editing = Nothing
                              }
                            , Http.send SubmitEditing (authPutFieldRequest endpoint token editing.id (fieldNameFor editing.column) fieldValue)
                            )

                        Nothing ->
                            ( { model | editing = Just <| setEditingError editing }
                            , Dom.focus inPlaceEditorId |> Task.attempt FocusResult
                            )

                -- This should never happen!!!
                Nothing ->
                    ( model, Cmd.none )

        SubmitFileUpload _ _ ->
            case model.editing of
                Just editing ->
                    if String.isEmpty editing.value then
                        ( { model | editing = Just <| setEditingError editing }
                        , Dom.focus inPlaceEditorId |> Task.attempt FocusResult
                        )
                    else
                        ( { model | editing = Nothing }
                        , initiateFileUpload inPlaceEditorId
                        )

                -- This should never happen!!!
                Nothing ->
                    ( model, Cmd.none )

        FileUploadStatus jsonVal ->
            Debug.log "We have liftoff!" ( model, Cmd.none )

        SubmitEditing (Err err) ->
            ( { model | errorMsg = Just <| httpErrToString err }
            , Cmd.none
            )

        SubmitEditing (Ok ()) ->
            ( { model | errorMsg = Nothing }
            , Cmd.none
            )

        CancelEditing ->
            ( { model | editing = Nothing }, Cmd.none )

        FocusResult result ->
            ( model, Cmd.none )

        SwitchSort column ->
            ( switchSort column model, Cmd.none )

        AddBannerClick ->
            ( { model | errorMsg = Nothing, editing = Nothing, isLoading = True }
            , Http.send AddBanner (authPostRequestExpectJson endpoint token bannerDecoder)
            )

        AddBanner (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| httpErrToString err }
            , Cmd.none
            )

        AddBanner (Ok banner) ->
            ( { model | banners = deserialize banner :: model.banners, isLoading = False, errorMsg = Nothing }
            , Cmd.none
            )

        LoadBannersClick ->
            ( { model | banners = [], isLoading = True, errorMsg = Nothing, editing = Nothing, sortOrder = Nothing }
            , Http.send LoadBanners (authGetRequestExpectJson endpoint token (list bannerDecoder))
            )

        LoadBanners (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| httpErrToString err }
            , Cmd.none
            )

        LoadBanners (Ok banners) ->
            ( { model | banners = List.map deserialize banners, errorMsg = Nothing, isLoading = False }
            , Cmd.none
            )

        DeleteBannerClick id ->
            ( { model | errorMsg = Nothing, editing = Nothing, isLoading = True }
            , Http.send (DeleteBanner id) (authDeleteRequest endpoint token id)
            )

        DeleteBanner _ (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| httpErrToString err }
            , Cmd.none
            )

        DeleteBanner id (Ok ()) ->
            ( { model
                | banners = List.filter ((/=) id << .id) model.banners
                , errorMsg = Nothing
                , isLoading = False
              }
            , Cmd.none
            )

        CloseErrorMsg ->
            ( { model | errorMsg = Nothing }, Cmd.none )

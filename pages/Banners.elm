port module Banners exposing
    ( Banner
    , Column(..)
    , Editing
    , Model
    , Msg(..)
    , SortOrder(..)
    , Validator
    , init
    , fileUploadStatus
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

-- import Debug exposing (log)
import Global exposing (..)
import Image exposing (..)
import Http
import Json.Decode exposing (Decoder, bool, int, list, null, nullable, oneOf, string, succeed, field, decodeValue)
import Json.Decode.Pipeline exposing (required)
import Task
import Browser.Dom as Dom
import Url
import Paths


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
    | FileUploadStatus Json.Decode.Value
    | SubmitEditing (Result Http.Error ())
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
    | CloseUploadErrorMsg


type alias Banner =
    { id : Int
    , isSilent : Bool
    , mStartDate : Maybe SimpleDate
    , mEndDate : Maybe SimpleDate
    , mImage : Maybe Image
    , mUrl : Maybe String
    , weight : Int
    }


type alias Editing =
    { id : Int
    , column : Column
    , value : String
    , isError : Bool
    , mUploadStatus : Maybe UploadStatus
    }


type alias Model =
    { isLoading : Bool
    , errorMsg : Maybe String
    , banners : List Banner
    , editing : Maybe Editing
    , sortOrder : Maybe ( Column, SortOrder )
    }


port initiateFileUpload : ( String, String ) -> Cmd msg


port fileUploadStatus : (Json.Decode.Value -> msg) -> Sub msg


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


bannerDecoder : Decoder Banner
bannerDecoder = 
    succeed Banner
        |> required "id" int
        |> required "isSilent" bool
        |> required "startDate" (oneOf [ simpleDateDecoder, null Nothing ])
        |> required "endDate" (oneOf [ simpleDateDecoder, null Nothing ])
        |> required "image" (oneOf [ mImageDecoder, null Nothing ])
        |> required "url" (nullable string)
        |> required "weight" int


setEditingError : Editing -> Editing
setEditingError editing =
    { editing | isError = True }


setEditingUploadStatus : Editing -> UploadStatus -> Maybe Editing
setEditingUploadStatus editing uploadStatus =
    Just { editing | isError = False, mUploadStatus = Just uploadStatus }


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
            "is_silent"

        StartDateColumn ->
            "start_date"

        EndDateColumn ->
            "end_date"

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
            if v > 0 then
                Just <| String.fromInt v
            else
                Nothing

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
    Maybe.map (\_ -> strVal) (Url.fromString strVal)


modifyUrl : Modifier
modifyUrl newUrl banner =
    { banner
        | mUrl =
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
        Maybe.map dateToString (stringToDate dateStr)


modifyDate : Column -> Modifier
modifyDate column newDateStr banner =
    let
        mNewDate =
            stringToDate newDateStr
    in
    if column == StartDateColumn then
        { banner | mStartDate = mNewDate }

    else
        { banner | mEndDate = mNewDate }


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
            compareMaybeDates b1.mStartDate b2.mStartDate

        EndDateColumn ->
            compareMaybeDates b1.mEndDate b2.mEndDate

        UrlColumn ->
            compare (Maybe.withDefault "" b1.mUrl) (Maybe.withDefault "" b2.mUrl)

        WeightColumn ->
            compare b1.weight b2.weight

        ActionsColumn ->
            EQ


switchSort : Column -> Model -> Model
switchSort column oldModel =
    let
        newBanners =
            case oldModel.sortOrder of
                Just ( oldColumn, _ ) ->
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
            , Http.send SubmitEditing (authPutFieldRequest endpoint token id (fieldNameFor SilentColumn) (boolToString checked))
            )

        StartEditing id column value ->
            ( { model | editing = Just (Editing id column value False Nothing) }
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
                        -- /banners/:id/upload
                        ( { model | editing = setEditingUploadStatus editing (Uploading 0) }
                        , initiateFileUpload ( inPlaceEditorId, Paths.api (endpoint ++ [String.fromInt editing.id, "upload"]) )
                        )

                -- This should never happen!!!
                Nothing ->
                    ( model, Cmd.none )

        FileUploadStatus jsonVal ->
            case model.editing of
                Just editing ->
                    case decodeValue uploadStatusDecoder jsonVal of
                        Ok (Uploading progress) ->
                            ( { model | editing = setEditingUploadStatus editing (Uploading progress) }
                            , Cmd.none )

                        Ok (UploadFinished (Err err)) ->
                            ( { model | editing = setEditingUploadStatus editing (UploadFinished (Err err)) }
                            , Cmd.none )

                        Ok (UploadFinished (Ok image)) ->
                            let
                                updateImage : Banner -> Banner
                                updateImage banner =
                                    if editing.id == banner.id then
                                        { banner | mImage = Just image }

                                    else
                                        banner
                                    
                            in                                    
                                ( { model | editing = Nothing, banners = List.map updateImage model.banners }
                                , Cmd.none )

                        Err _ ->
                            ( { model | editing = setEditingUploadStatus editing (UploadFinished <| Err "Nie zrozumiałem odpowiedzi serwera… :(") }
                            , Cmd.none )

                -- This should never happen!!!
                Nothing ->
                    ( model, Cmd.none )

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

        FocusResult _ ->
            ( model, Cmd.none )

        SwitchSort column ->
            ( switchSort column model, Cmd.none )

        AddBannerClick ->
            ( { model | errorMsg = Nothing, editing = Nothing, isLoading = True }
            , Http.send AddBanner (authPostRequestExpectJson (endpoint ++ [ "new" ]) token bannerDecoder)
            )

        AddBanner (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| httpErrToString err }
            , Cmd.none
            )

        AddBanner (Ok banner) ->
            ( { model | banners = banner :: model.banners, isLoading = False, errorMsg = Nothing }
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
            ( { model | banners = banners, errorMsg = Nothing, isLoading = False }
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

        CloseUploadErrorMsg ->
            ( { model | editing = Nothing }, Cmd.none )

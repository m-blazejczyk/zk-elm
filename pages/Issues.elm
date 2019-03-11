module Issues exposing
    ( Issue
    , IssueLang
    , EditableItem(..)
    , Editing
    , Model
    , Msg(..)
    , init
    , switchToPageCmd
    , update
    )


import Debug exposing (log)
import Global exposing (..)
import Http
--import Json.Decode exposing (Decoder, bool, int, list, null, nullable, oneOf, string, succeed, field, decodeValue)
--import Json.Decode.Pipeline exposing (required)
import Result
import Task
import Browser.Dom as Dom
import Url
import Paths


type Msg
    = LoadIssuesClick
    | LoadIssues (Result Http.Error (List Issue))
    | AddIssueClick
    | AddIssue (Result Http.Error Issue)
    | CloseErrorMsg


type Availability
    = InPreparation
    | Available
    | ReprintAvailable
    | OutOfPrint


type alias Issue =
    { id : Int
    , availability : Availability
    , mPrice : Maybe String
    , pl : IssueLang
    , en : IssueLang
    --, mImageBig : Maybe Image
    --, mImageMedium : Maybe Image
    --, mImageSmall : Maybe Image
    }


type alias IssueLang =
    { id : Int
    , isPublished : Bool
    , hasTOC : Bool
    , mPubDate : Maybe SimpleDate
    , mTopic : Maybe String
    , mEditorial : Maybe String
    , mSignature : Maybe String
    }


type EditableItem
    = Dummy


type alias Editing =
    { id : Int
    , item : EditableItem
    , value : String
    , isError : Bool
    }


type alias Model =
    { isLoading : Bool
    , errorMsg : Maybe String
    , issues : List Issue
    , editing : Maybe Editing
    }


init : Model
init =
    Model False Nothing [] Nothing


endpoint : List String
endpoint = [ "issues" ]


newIssue : Issue
newIssue =
    let
        newLang = IssueLang -1 False False Nothing Nothing Nothing Nothing
    in
    Issue -1 InPreparation Nothing newLang newLang


switchToPageCmd : Cmd Msg
switchToPageCmd =
    toCmd LoadIssuesClick


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model token =
    case msg of
        LoadIssuesClick ->
            ( { model | issues = [], isLoading = True, errorMsg = Nothing, editing = Nothing }
            , Cmd.none --Http.send LoadIssues (authGetRequestExpectJson endpoint token (list issueDecoder))
            )

        LoadIssues (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| httpErrToString err }
            , Cmd.none
            )

        LoadIssues (Ok issues) ->
            ( { model | issues = issues, errorMsg = Nothing, isLoading = False }
            , Cmd.none
            )

        AddIssueClick ->
            ( { model | errorMsg = Nothing, editing = Nothing, isLoading = True }
            , Cmd.none --Http.send AddIssue (authPostRequestExpectJson (endpoint ++ [ "new" ]) token issueDecoder)
            )

        AddIssue (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| httpErrToString err }
            , Cmd.none
            )

        AddIssue (Ok issue) ->
            ( { model | issues = issue :: model.issues, isLoading = False, errorMsg = Nothing }
            , Cmd.none
            )

        CloseErrorMsg ->
            ( { model | errorMsg = Nothing }, Cmd.none )

module Issues exposing
    ( Issue
    , IssueLang
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
    | AddIssueClick
    | CloseErrorMsg


type alias Issue =
    { id : Int
    }


type alias IssueLang =
    { id : Int
    }


type alias Model =
    { isLoading : Bool
    , errorMsg : Maybe String
    , issues : List Issue
    }


init : Model
init =
    Model False Nothing []


endpoint : List String
endpoint = [ "issues" ]


newIssue : Issue
newIssue =
    Issue -1


switchToPageCmd : Cmd Msg
switchToPageCmd =
    toCmd LoadIssuesClick


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model token =
    case msg of
        LoadIssuesClick ->
            ( model, Cmd.none )

        AddIssueClick ->
            ( model, Cmd.none )

        CloseErrorMsg ->
            ( { model | errorMsg = Nothing }, Cmd.none )

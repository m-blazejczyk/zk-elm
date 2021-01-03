port module Issues exposing
    ( Issue
    , IssueLang
    , Availability(..)
    , formValuesReceiver
    , IssueEditable
    , IssueLangEditable
    , issuePubDate
    , issueTopic
    , issueEditorial
    , issueSignature
    , Model
    , Msg(..)
    , init
    , switchToPageCmd
    , update
    )


-- import Debug exposing (log)
import Global exposing (..)
import Http
import Json.Decode exposing (Decoder, bool, int, list, nullable, string, succeed, fail, andThen)
import Json.Decode.Pipeline exposing (required)
import List
import Dict
-- import Result
-- import Task
-- import Browser.Dom as Dom
-- import Url
-- import Paths


port askForFormValues : () -> Cmd msg


port formValuesReceiver : (Json.Decode.Value -> msg) -> Sub msg


type Msg
    = LoadIssuesClick
    | LoadIssues (Result Http.Error (List Issue))
    | AddIssueClick
    | AddIssue (Result Http.Error Issue)
    | CloseErrorMsg
    | StartEditing Int
    | SubmitEdit
    | ReceiveFormValues Json.Decode.Value
    | StopEditing


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
    , mPubDate : Maybe String
    , mTopic : Maybe String
    , mEditorial : Maybe String
    , mSignature : Maybe String
    }


type alias IssueEditable =
    { isNew : Bool
    , id : String
    , availability : Availability
    , price : String
    , pl : IssueLangEditable
    , en : IssueLangEditable
    }


type alias IssueLangEditable =
    { pubDate : String
    , topic : String
    }


type alias Model =
    { isLoading : Bool
    , errorMsg : Maybe String
    , issues : Dict.Dict Int Issue
    , latestIssueId : Int
    , mEditing : Maybe IssueEditable
    }


issuePubDate : IssueLang -> String
issuePubDate issueLang =
    issueLang.mPubDate |> Maybe.withDefault "Brak daty publikacji"


issueTopic : IssueLang -> String
issueTopic issueLang =
    issueLang.mTopic |> Maybe.withDefault "Brak tytułu"


issueEditorial : IssueLang -> String
issueEditorial issueLang =
    issueLang.mEditorial |> Maybe.withDefault "Brak wstępniaka"


issueSignature : IssueLang -> String
issueSignature issueLang =
    issueLang.mSignature |> Maybe.withDefault "Brak podpisu wstępniaka"


init : Model
init =
    Model False Nothing Dict.empty 0 Nothing


endpoint : List String
endpoint = [ "issues" ]


newIssue : Issue
newIssue =
    let
        newLang = IssueLang -1 False False Nothing Nothing Nothing Nothing
    in
    Issue -1 InPreparation Nothing newLang newLang


availabilityDecoder : Decoder Availability
availabilityDecoder = 
    int |> andThen (\availability ->
           case availability of
                1 ->
                    succeed InPreparation
                2 ->
                    succeed Available
                3 ->
                    succeed ReprintAvailable
                4 ->
                    succeed OutOfPrint
                somethingElse ->
                    fail <| "Unknown availability: " ++ String.fromInt somethingElse
        )


issueLangDecoder : Decoder IssueLang
issueLangDecoder = 
    succeed IssueLang
        |> required "id" int
        |> required "is_published" bool
        |> required "has_toc" bool
        |> required "pub_date" (nullable string)
        |> required "topic" (nullable string)
        |> required "editorial" (nullable string)
        |> required "editorial_sig" (nullable string)

        
issueDecoder : Decoder Issue
issueDecoder = 
    succeed Issue
        |> required "id" int
        |> required "availability" availabilityDecoder
        |> required "price" (nullable string)
        |> required "pl" issueLangDecoder
        |> required "en" issueLangDecoder


issueLangToEditable : IssueLang -> IssueLangEditable
issueLangToEditable lang =
    IssueLangEditable
        (Maybe.withDefault "" lang.mPubDate)
        (Maybe.withDefault "" lang.mTopic)


issueToEditable : Issue -> IssueEditable
issueToEditable issue =
    IssueEditable
        False
        (String.fromInt issue.id)
        issue.availability
        (Maybe.withDefault "" issue.mPrice)
        (issueLangToEditable issue.pl)
        (issueLangToEditable issue.en)


switchToPageCmd : Cmd Msg
switchToPageCmd =
    toCmd LoadIssuesClick


update : Msg -> Model -> String -> ( Model, Cmd Msg )
update msg model token =
    let
        processIssuesList issue ( issuesDict, maxId ) = 
            (  Dict.insert issue.id issue issuesDict , max issue.id maxId )
    in
    case msg of
        LoadIssuesClick ->
            ( { model | issues = Dict.empty, isLoading = True, errorMsg = Nothing, mEditing = Nothing }
            , Http.send LoadIssues (authGetRequestExpectJson endpoint token (list issueDecoder))
            )

        LoadIssues (Err err) ->
            ( { model | isLoading = False, errorMsg = Just <| httpErrToString err }
            , Cmd.none
            )

        LoadIssues (Ok issuesList) ->
            let
                ( issues, latestIssueId ) = List.foldl processIssuesList ( Dict.empty, 0 ) issuesList
            in
            ( { model | issues = issues, latestIssueId = latestIssueId, errorMsg = Nothing, isLoading = False }
            , Cmd.none
            )

        AddIssueClick ->
            ( { model | errorMsg = Nothing, isLoading = True }
            , Cmd.none --Http.send AddIssue (authPostRequestExpectJson (endpoint ++ [ "new" ]) token issueDecoder)
            )

        AddIssue (Err err) ->
            ( { model | errorMsg = Just <| httpErrToString err, isLoading = False }
            , Cmd.none
            )

        AddIssue (Ok issue) ->
            ( { model | issues = Dict.insert issue.id issue model.issues, latestIssueId = issue.id, isLoading = False, errorMsg = Nothing }
            , Cmd.none
            )

        CloseErrorMsg ->
            ( { model | errorMsg = Nothing }
            , Cmd.none
            )

        StartEditing id ->
            ( { model | errorMsg = Nothing, mEditing = Maybe.map issueToEditable (Dict.get id model.issues) }
            , Cmd.none
            )

        SubmitEdit ->
            ( { model | errorMsg = Nothing }
            , askForFormValues () 
            )

        ReceiveFormValues jsonVal ->
            case model.mEditing of
                Just editing ->
                    ( { model | errorMsg = Just "Jest!" }, Cmd.none )

                -- This should never happen!!!
                Nothing ->
                    ( model, Cmd.none )

        StopEditing ->
            ( { model | errorMsg = Nothing, mEditing = Nothing }
            , Cmd.none
            )

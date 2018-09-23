module Global exposing
    ( boolToString
    , httpErrToString
    , SimpleDate
    , authDeleteRequest
    , authGetRequestExpectJson
    , authPostRequestExpectJson
    , authPutFieldRequest
    , authRequestExpectJson
    , compareMaybeDates
    , dateToString
    , domain
    , maybeIsJust
    , stringToDate
    , toCmd
    )

import Array
import Time exposing (Month(..))
import Http
import Json.Decode exposing (Decoder)
import Maybe exposing (..)
import Result
import Task
import Url.Builder as Url


type alias SimpleDate =
    { day : Int
    , month : Time.Month
    , year : Int
    }


boolToString : Bool -> String
boolToString b = 
    if b then "True" else "False"


httpErrToString : Http.Error -> String
httpErrToString err =
    case err of
        Http.BadUrl url ->
            "BadUrl " ++ url

        Http.Timeout ->
            "Timeout"

        Http.NetworkError ->
            "NetworkError"

        Http.BadStatus response ->
            "BadStatus " ++ String.fromInt response.status.code

        Http.BadPayload reason _ ->
            "BadPayload: " ++ reason


domain : String
domain =
    "https://red.zeszytykomiksowe.org"


expectHttpCodeResponse : Http.Expect ()
expectHttpCodeResponse =
    Http.expectStringResponse
        (\response ->
            if response.status.code == 200 then
                Ok ()

            else
                Err response.status.message
        )


authHeader : String -> List Http.Header
authHeader token =
    [ Http.header "Authorization" token ]


authGetRequestExpectJson : List String -> String -> Decoder a -> Http.Request a
authGetRequestExpectJson =
    authRequestExpectJson "GET"


authPostRequestExpectJson : List String -> String -> Decoder a -> Http.Request a
authPostRequestExpectJson =
    authRequestExpectJson "POST"


authRequestExpectJson : String -> List String -> String -> Decoder a -> Http.Request a
authRequestExpectJson method endpoint token decoder =
    { method = method
    , headers = authHeader token
    , url = Url.crossOrigin domain endpoint []
    , body = Http.emptyBody
    , expect = Http.expectJson decoder
    , timeout = Nothing
    , withCredentials = False
    }
        |> Http.request


authDeleteRequest : List String -> String -> Int -> Http.Request ()
authDeleteRequest endpoint token id =
    { method = "DELETE"
    , headers = authHeader token
    , url = Url.crossOrigin domain (endpoint ++ [ String.fromInt id ]) []
    , body = Http.emptyBody
    , expect = expectHttpCodeResponse
    , timeout = Nothing
    , withCredentials = False
    }
        |> Http.request


authPutFieldRequest : List String -> String -> Int -> String -> String -> Http.Request ()
authPutFieldRequest endpoint token id fieldName fieldValue =
    { method = "POST"
    , headers = authHeader token
    , url = Url.crossOrigin domain (endpoint ++ [ String.fromInt id, "edit" ]) []
    , body = Http.multipartBody [ Http.stringPart fieldName fieldValue ]
    , expect = expectHttpCodeResponse
    , timeout = Nothing
    , withCredentials = False
    }
        |> Http.request


{-| A command to generate a message without performing any action.
This is useful for implementing components that generate events in the manner
of HTML elements, but where the event fires from within Elm code, rather than
by an external trigger.
-}
toCmd : msg -> Cmd msg
toCmd msg =
    Task.perform identity (Task.succeed msg)


maybeIsJust : Maybe a -> Bool
maybeIsJust maybe =
    case maybe of
        Just _ ->
            True

        Nothing ->
            False


maxDayOfMonth : Month -> Int
maxDayOfMonth month =
    case month of
        Jan ->
            31

        Feb ->
            29

        Mar ->
            31

        Apr ->
            30

        May ->
            31

        Jun ->
            30

        Jul ->
            31

        Aug ->
            31

        Sep ->
            30

        Oct ->
            31

        Nov ->
            30

        Dec ->
            31


monthToInt : Month -> Int
monthToInt month =
    case month of
        Jan ->
            1

        Feb ->
            2

        Mar ->
            3

        Apr ->
            4

        May ->
            5

        Jun ->
            6

        Jul ->
            7

        Aug ->
            8

        Sep ->
            9

        Oct ->
            10

        Nov ->
            11

        Dec ->
            12


intToMonth : Int -> Maybe Month
intToMonth monthInt =
    case monthInt of
        1 ->
            Just Jan

        2 ->
            Just Feb

        3 ->
            Just Mar

        4 ->
            Just Apr

        5 ->
            Just May

        6 ->
            Just Jun

        7 ->
            Just Jul

        8 ->
            Just Aug

        9 ->
            Just Sep

        10 ->
            Just Oct

        11 ->
            Just Nov

        12 ->
            Just Dec

        _ ->
            Nothing


dateToString : SimpleDate -> String
dateToString date =
    String.fromInt (.year date) ++ "-" 
 ++ String.fromInt (monthToInt (.month date)) ++ "-"
 ++ String.fromInt (.day date)


stringToDate : String -> Maybe SimpleDate
stringToDate dateStr =
    let
        dateArr =
            Array.fromList <| String.split "-" dateStr

        validateYear y =
            if y >= 1800 && y <= 2050 then
                Just y

            else
                Nothing

        validateMonth m =
            if m >= 1 && m <= 12 then
                Just m

            else
                Nothing

        validateDay mMonth1 d =
            case mMonth1 of
                Just month ->
                    if d >= 1 && d <= maxDayOfMonth month then
                        Just d

                    else
                        Nothing

                Nothing ->
                    Nothing

        mYear =
            Array.get 0 dateArr
                |> andThen String.toInt
                |> andThen validateYear

        mMonth =
            Array.get 1 dateArr
                |> andThen String.toInt
                |> andThen validateMonth
                |> andThen intToMonth

        mDay =
            Array.get 2 dateArr
                |> andThen String.toInt
                |> andThen (validateDay mMonth)
    in
    case mYear of
        Just y ->
            case mMonth of
                Just m ->
                    case mDay of
                        Just d ->
                            Just <| SimpleDate d m y

                        Nothing ->
                            Nothing

                Nothing ->
                    Nothing

        Nothing ->
            Nothing


compareMaybeDates : Maybe SimpleDate -> Maybe SimpleDate -> Order
compareMaybeDates md1 md2 =
    let
        dateToNum sd =
            sd.year * 500 + monthToInt sd.month * 35 + sd.day

        maybeDateToNum msd =
            Maybe.withDefault 0 (Maybe.map dateToNum msd)
    in
    compare (maybeDateToNum md1) (maybeDateToNum md2)

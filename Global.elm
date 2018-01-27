module Global exposing (domain, maybeIsJust, SimpleDate, dateToString, stringToDate, compareMaybeDates)

import Array
import Date exposing (Month (..))
import Maybe exposing (..)
import Result


type alias SimpleDate =
    { day : Int
    , month : Date.Month
    , year : Int
    }


domain : String
domain =
    "https://red.zeszytykomiksowe.org/"


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
    toString (.year date) ++ "-" ++ toString (monthToInt (.month date)) ++ "-" ++ toString (.day date)


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

        validateDay mMonth d =
            case mMonth of
                Just month ->
                    if d >= 1 && d <= maxDayOfMonth month then
                        Just d
                    else 
                        Nothing
                Nothing ->
                    Nothing

        mYear = 
            Array.get 0 dateArr
                |> andThen (Result.toMaybe << String.toInt)
                |> andThen validateYear

        mMonth = 
            Array.get 1 dateArr
                |> andThen (Result.toMaybe << String.toInt)
                |> andThen validateMonth
                |> andThen intToMonth

        mDay = 
            Array.get 2 dateArr
                |> andThen (Result.toMaybe << String.toInt)
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
        dateToNum sd = sd.year * 500 + ( monthToInt sd.month ) * 35 + sd.day

        maybeDateToNum msd = Maybe.withDefault 0 ( Maybe.map dateToNum msd )

    in

        compare ( maybeDateToNum md1 ) ( maybeDateToNum md2 )

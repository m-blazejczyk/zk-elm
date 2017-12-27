module Global exposing (domain, dateToStringPl)

import Date exposing (..)


domain : String
domain =
    "https://red.zeszytykomiksowe.org/"


monthToStringPl : Month -> String
monthToStringPl month =
    case month of
        Jan ->
            "Sty"

        Feb ->
            "Lut"

        Mar ->
            "Mar"

        Apr ->
            "Kwi"

        May ->
            "Maj"

        Jun ->
            "Cze"

        Jul ->
            "Lip"

        Aug ->
            "Sie"

        Sep ->
            "Wrz"

        Oct ->
            "Paź"

        Nov ->
            "Lis"

        Dec ->
            "Gru"


dateToStringPl : Date -> String
dateToStringPl date =
    (toString <| day date) ++ " " ++ (monthToStringPl <| month date) ++ " " ++ (toString <| year date)

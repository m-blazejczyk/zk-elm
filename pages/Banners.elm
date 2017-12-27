module Banners exposing (..)

import Date exposing (Date)
import Result


type Msg
    = Test


type alias Banner =
    { id : Int
    , isSilent : Bool
    , startDate : Maybe Date
    , endDate : Maybe Date
    , image : String
    , imageH : Int
    , imageW : Int
    , url : String
    , weight : Int
    }


type alias Model =
    { banners : List Banner
    }


init : Model
init =
    Model [ Banner 1 False Nothing (Result.toMaybe <| Date.fromString "2018/1/15") "http://www.zeszytykomiksowe.org/aktualnosci/bannery/dydaktyczny-potencjal.jpg" 89 200 "http://fundacja-ikp.pl/wydawnictwo/" 10 ]

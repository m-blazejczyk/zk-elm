module Banners exposing (..)

import Date exposing ( Date )
import Result


type Msg
    = Test


type alias Banner =
    { silent : Bool
    , startDate : Maybe Date
    , endDate : Maybe Date
    , image : String
    , imageH : Int
    , imageW : Int
    , url : String
    }

type alias Model =
    { bannerText : String
    , banners : List Banner
    }


init : Model
init = 
    Model "Test" [ Banner False Nothing ( Result.toMaybe <| Date.fromString "2018/1/15" ) "http://www.zeszytykomiksowe.org/aktualnosci/bannery/dydaktyczny-potencjal.jpg" 89 200 "http://fundacja-ikp.pl/wydawnictwo/" ]

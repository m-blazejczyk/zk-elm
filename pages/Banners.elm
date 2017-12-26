module Banners exposing (..)


type Msg
    = Test


type alias Model =
    { bannerText : String
    }


init : Model
init = 
    Model "Test"

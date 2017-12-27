module BannersView exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

-- Model, Msg, ...
import Banners exposing (..)


view : Model -> Html Msg
view model =
    p [] [ text <| Basics.toString model.banners ]

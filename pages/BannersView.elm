module BannersView exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

-- Model, Msg, ...
import Banners exposing (..)


view : Model -> Html Msg
view model =
    table [ class "table" ]
        [ thead []
            [ tr []
                [ th [] [ span [ class "glyphicon glyphicon-ban-circle" ] [] ]
                , th [] [ text "Obrazek" ]
                , th [] [ text "Wyświetlaj od…" ]
                , th [] [ text "…do" ]
                , th [] [ text "Link" ]
                , th [] [ text "" ]
                ]
            ]
        , tbody []
            [ tr []
                [ td [] [ text "1" ]
                , td [] [ text "2" ]
                , td [] [ text "3" ]
                , td [] [ text "4" ]
                , td [] [ text "5" ]
                , td [] [ text "6" ]
                ]
            ]
        ]

module BannersView exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Ui exposing (..)
-- Model, Msg, ...
import Banners exposing (..)


view : Model -> Html Msg
view model =
    table [ class "table table-bordered" ]
        [ thead []
            [ tr []
                [ th [] [ glyphicon "info-sign" SpaceRight, glyphicon "ban-circle" NoSpace ]
                , th [] [ glyphicon "info-sign" SpaceRight, glyphicon "sort" SpaceRight, text "Obrazek" ]
                , th [] [ glyphicon "info-sign" SpaceRight, glyphicon "sort" SpaceRight, text "Wyświetlaj od…" ]
                , th [] [ glyphicon "info-sign" SpaceRight, glyphicon "sort" SpaceRight, text "…do" ]
                , th [] [ glyphicon "info-sign" SpaceRight, glyphicon "sort" SpaceRight, text "Link" ]
                , th [] [ glyphicon "info-sign" SpaceRight, glyphicon "sort" SpaceRight, text "Waga" ]
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
                , td [] [ button [ class "btn btn-danger btn-sm" ] [ glyphicon "trash" NoSpace ] ]
                ]
            ]
        ]

module Ui exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type Space
  = SpaceRight
  | NoSpace


glyphicon : String -> Space -> Html msg
glyphicon glyph space =
    let
      spaceStyle =
          case space of
              SpaceRight ->
                  style [ ( "margin-right", "10px" ) ]
              NoSpace ->
                  style []
        
    in
        span [ class "glyphicon", class ( "glyphicon-" ++ glyph ), spaceStyle ] []
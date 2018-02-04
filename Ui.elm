module Ui exposing (Space(..), glyphicon, glyphiconInfo, viewErrorMsg)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)


type Space
    = SpaceRight
    | NoSpace


spaceStyle : Space -> Attribute msg
spaceStyle space =
    case space of
        SpaceRight ->
            style [ ( "margin-right", "10px" ) ]

        NoSpace ->
            style []


glyphicon : String -> Space -> Html msg
glyphicon glyph space =
    span [ class "glyphicon", class ("glyphicon-" ++ glyph), spaceStyle space ] []


glyphiconInfo : Space -> String -> Html msg
glyphiconInfo space tooltip =
    span [ class "glyphicon", class "glyphicon-info-sign", spaceStyle space ]
        [ span [ class "tooltip-text tooltip-glyphicon" ] [ text tooltip ] ]


viewErrorMsg : Maybe String -> msg -> Html msg
viewErrorMsg mError msg =
    case mError of
        Just error ->
            div [ class "alert alert-danger" ]
                [ button [ type_ "button", class "close", onClick msg ] [ text "×" ]
                , text error ]

        Nothing ->
            text ""

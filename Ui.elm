module Ui exposing (Space(..), glyphicon, glyphiconInfo, viewErrorMsg, viewSpinner)

import Paths
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type Space
    = SpaceRight
    | NoSpace


spaceStyle : Space -> List (Attribute msg)
spaceStyle space =
    case space of
        SpaceRight ->
            [ style "margin-right" "10px"]

        NoSpace ->
            []


glyphicon : String -> Space -> Html msg
glyphicon glyph space =
    span ([ class "glyphicon", class ("glyphicon-" ++ glyph) ] ++ spaceStyle space) []


glyphiconInfo : Space -> String -> Html msg
glyphiconInfo space tooltip =
    span ([ class "glyphicon", class "glyphicon-info-sign" ] ++ spaceStyle space)
        [ span [ class "tooltip-text tooltip-glyphicon" ] [ text tooltip ] ]


viewErrorMsg : Maybe String -> msg -> Html msg
viewErrorMsg mError msg =
    case mError of
        Just error ->
            div [ class "alert alert-danger" ]
                [ button [ type_ "button", class "close", onClick msg ] [ text "×" ]
                , text error
                ]

        Nothing ->
            text ""


viewSpinner : Bool -> Html msg
viewSpinner shouldView =
    if shouldView then
        div [ style "width" "100%" ]
            [ img [ src <| Paths.zkRoot [ "ajax-loader.gif" ], width 32, height 32, class "center-block", style "margin-bottom" "20px" ] [] ]

    else
        text ""

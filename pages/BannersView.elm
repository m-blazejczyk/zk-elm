module BannersView exposing (view)

import Date exposing (Date)
import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Global exposing (..)
import Ui exposing (..)


-- Model, Msg, ...

import Banners exposing (..)


silentColumnTooltip : String
silentColumnTooltip =
    "Zaznaczenie pola wyboru w tej kolumnie spowoduje, że dany banner nie będzie wyświetlany."


startDateColumnTooltip : String
startDateColumnTooltip =
    "Data, od której banner powinien być wyświetlany.  Jeśli jest jej brak, banner będzie wyświetlany od momentu jego utworzenia."


endDateColumnTooltip : String
endDateColumnTooltip =
    "Data, do której banner powinien być wyświetlany.  Jeśli jest jej brak, banner będzie wyświetlany po wsze czasy."


urlColumnTooltip : String
urlColumnTooltip =
    "Adres strony, do której będzie odsyłać obrazek bannera.  Jeśli jest pusty to banner nie będzie linkiem."


weightColumnTooltip : String
weightColumnTooltip =
    "Liczba, oznaczająca, jak często ten banner ma się pojawiać na stronie.  Domyślnie - 10.  5 oznacza „dwa razy rzadziej niż normalnie”, 20 oznacza „dwa razy częściej niż normalnie”."


columnWidth : Column -> Maybe Int
columnWidth col =
    case col of
        SilentColumn ->
            Just 80

        ImageColumn ->
            Nothing

        StartDateColumn ->
            Just 100

        EndDateColumn ->
            Just 100

        UrlColumn ->
            Nothing

        WeightColumn ->
            Just 100

        ActionsColumn ->
            Nothing


columnStyle : Column -> List (String, String)
columnStyle col = 
    case columnWidth col of
        Just width ->
            [ ( "width", (toString width) ++ "px" ) ]

        Nothing ->
            []


viewImage : Banner -> Html Msg
viewImage data =
    if String.isEmpty data.image then
        text "Brak obrazka"
    else
        div [ style [ ( "text-align", "center" ) ] ]
            [ img [ src data.image, width data.imageW, height data.imageH ] []
            , br [] []
            , span [] [ text (toString data.imageW ++ " × " ++ toString data.imageH ++ " px") ]
            ]


viewDate : Maybe Date -> String
viewDate mDate =
    case mDate of
        Just date ->
            dateToStringPl date

        Nothing ->
            "Brak daty"


viewUrl : String -> String
viewUrl url =
    let
        urlNoHttp =
            if String.startsWith "http://" url then
                String.dropLeft 7 url
            else if String.startsWith "https://" url then
                String.dropLeft 8 url
            else
                url
    in
        if String.isEmpty url then
            "Brak linka"
        else if String.length urlNoHttp < 20 then
            urlNoHttp
        else
            (String.left 20 urlNoHttp) ++ "…"


viewInputButtons: Html Msg
viewInputButtons = 
    div [ class "btn-group right-align" ]
        [ button [ class "btn btn-default btn-sm", style [ ( "color", "green" ) ] ] [ glyphicon "ok" NoSpace ]
        , button [ class "btn btn-default btn-sm", style [ ( "color", "red" ) ] ]   [ glyphicon "remove" NoSpace ]
        ]


viewInputNormal: String -> Html Msg
viewInputNormal val =
    div [ class "form-group full-width-input" ]
        [ input [ type_ "text", class "form-control", value val ] [] ]

viewInputWithError: String -> Html Msg
viewInputWithError val =
    div [ class "form-group has-error has-feedback full-width-input" ]
        [ input [ type_ "text", class "form-control", value val ] []
        , span [ class "glyphicon glyphicon-exclamation-sign form-control-feedback" ] []
        ]

viewWeight : Int -> Html Msg
viewWeight weight =
    let
        viewInput =
            if weight > 10 then
                viewInputWithError <| toString weight
            else
                viewInputNormal <| toString weight
    in
        div [ class "full-width" ] [ viewInput, viewInputButtons ]


viewSingleBanner : Banner -> Html Msg
viewSingleBanner data =
    tr []
        [ td [ style <| columnStyle SilentColumn ] [ input [ type_ "checkBox", checked data.isSilent, onCheck (ChangeSilent data.id) ] [], text " Ukryj" ]
        , td [ style <| columnStyle ImageColumn ] [ viewImage data ]
        , td [ style <| columnStyle StartDateColumn ] [ text <| viewDate data.startDate ]
        , td [ style <| columnStyle EndDateColumn ] [ text <| viewDate data.endDate ]
        , td [ style <| columnStyle UrlColumn ] [ span [ class "with-tooltip" ] [ text <| viewUrl data.url, span [ class "tooltip-text tooltip-span" ] [ text data.url ] ] ]
        , td [ style <| columnStyle WeightColumn ] [ viewWeight data.weight ]
        , td [ style <| columnStyle ActionsColumn ] [ button [ class "btn btn-danger btn-sm" ] [ glyphicon "trash" NoSpace ] ]
        ]


view : Model -> Html Msg
view model =
    table [ class "table table-bordered" ]
        [ thead []
            [ tr []
                [ th [ style <| columnStyle SilentColumn ]    [ glyphiconInfo SpaceRight silentColumnTooltip, text "Ukryj" ]
                , th [ style <| columnStyle ImageColumn ]     [ text "Obrazek" ]
                , th [ style <| columnStyle StartDateColumn ] [ glyphiconInfo SpaceRight startDateColumnTooltip, glyphicon "sort" SpaceRight, text "Od…" ]
                , th [ style <| columnStyle EndDateColumn ]   [ glyphiconInfo SpaceRight endDateColumnTooltip, glyphicon "sort" SpaceRight, text "…do" ]
                , th [ style <| columnStyle UrlColumn ]       [ glyphiconInfo SpaceRight urlColumnTooltip, glyphicon "sort" SpaceRight, text "Link" ]
                , th [ style <| columnStyle WeightColumn ]    [ glyphiconInfo SpaceRight weightColumnTooltip, glyphicon "sort" SpaceRight, text "Waga" ]
                , th [ style <| columnStyle ActionsColumn ]   [ text "" ]
                ]
            ]
        , tbody [] (List.map viewSingleBanner model.banners)
        ]

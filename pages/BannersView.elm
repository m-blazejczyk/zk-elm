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


viewSingleBanner : Banner -> Html Msg
viewSingleBanner data =
    tr []
        [ td [] [ input [ type_ "checkBox", checked data.isSilent, onCheck (ChangeSilent data.id) ] [], text " Ukryj" ]
        , td [] [ viewImage data ]
        , td [] [ text <| viewDate data.startDate ]
        , td [] [ text <| viewDate data.endDate ]
        , td [] [ text <| viewUrl data.url ]
        , td [] [ text <| toString data.weight ]
        , td [] [ button [ class "btn btn-danger btn-sm" ] [ glyphicon "trash" NoSpace ] ]
        ]


view : Model -> Html Msg
view model =
    table [ class "table table-bordered" ]
        [ thead []
            [ tr []
                [ th [] [ glyphiconInfo NoSpace silentColumnTooltip ]
                , th [] [ text "Obrazek" ]
                , th [] [ glyphiconInfo SpaceRight startDateColumnTooltip, glyphicon "sort" SpaceRight, text "Wyświetlaj od…" ]
                , th [] [ glyphiconInfo SpaceRight endDateColumnTooltip, glyphicon "sort" SpaceRight, text "…do" ]
                , th [] [ glyphiconInfo SpaceRight urlColumnTooltip, glyphicon "sort" SpaceRight, text "Link" ]
                , th [] [ glyphiconInfo SpaceRight weightColumnTooltip, glyphicon "sort" SpaceRight, text "Waga" ]
                , th [] [ text "" ]
                ]
            ]
        , tbody [] (List.map viewSingleBanner model.banners)
        ]

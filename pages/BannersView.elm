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


viewInputButtons: Attribute Msg -> Html Msg
viewInputButtons onOkClick = 
    div [ class "btn-group right-align" ]
        [ button [ class "btn btn-default btn-sm", style [ ( "color", "green" ) ], onOkClick ]
            [ glyphicon "ok" NoSpace ]
        , button [ class "btn btn-default btn-sm", style [ ( "color", "red" ) ], onClick CancelEditing ]
            [ glyphicon "remove" NoSpace ]
        ]


viewInputWrapper: Attribute Msg -> Html Msg -> Html Msg
viewInputWrapper onOkClick content =
    div [ class "full-width" ]
        [ content, viewInputButtons onOkClick ]


stdEditingInputAttrs: List (Attribute Msg)
stdEditingInputAttrs = [ type_ "text", class "form-control", id "inPlaceEditor" ]


viewInputNormal: List (Attribute Msg) -> Attribute Msg -> Html Msg
viewInputNormal inputAttrs onOkClick =
    viewInputWrapper
        onOkClick
        (div [ class "form-group full-width-input" ]
            [ input (inputAttrs ++ stdEditingInputAttrs) [] ])


viewInputWithError: List (Attribute Msg) -> Attribute Msg -> Html Msg
viewInputWithError inputAttrs onOkClick =
    viewInputWrapper
        onOkClick
        (div [ class "form-group has-error has-feedback full-width-input" ]
            [ input (inputAttrs ++ stdEditingInputAttrs) []
            , span [ class "glyphicon glyphicon-exclamation-sign form-control-feedback" ] []
            ])


viewEditingInput: Maybe Editing -> Int -> Html Msg -> Column -> List (Attribute Msg) -> Attribute Msg -> Html Msg
viewEditingInput mEditing id nonEditingView column inputAttrs onOkClick =
    case mEditing of
        Just editing ->
            if editing.id == id && editing.column == column then
                if editing.isError then
                    viewInputWithError inputAttrs onOkClick
                else
                    viewInputNormal inputAttrs onOkClick
            else
                nonEditingView

        Nothing ->
            nonEditingView


viewWeight : Maybe Editing -> Banner -> Html Msg
viewWeight mEditing data =
    let
        weightAsString = toString data.weight

        nonEditingView =
            span [ onClick <| StartEditing data.id WeightColumn weightAsString ]
                [ text weightAsString ]

        validator val = False

        modifier _ banner = banner

    in

        viewEditingInput
            mEditing data.id nonEditingView WeightColumn
            [ maxlength 2, value weightAsString ]
            (onClick (ValidateEditing validator modifier))


shorterUrl : String -> String
shorterUrl url =
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


viewUrl : Maybe Editing -> Banner -> Html Msg
viewUrl mEditing data = 
    let
        nonEditingView =
            span [ class "with-tooltip", onClick <| StartEditing data.id UrlColumn data.url ]
                [ text <| shorterUrl data.url
                , span [ class "tooltip-text tooltip-span" ] [ text data.url ]
                ]
           
        validator val = False

        modifier _ banner = banner
 
    in
            
        viewEditingInput
            mEditing data.id nonEditingView UrlColumn
            [ maxlength 500, value data.url ]
            (onClick (ValidateEditing validator modifier))


viewSingleBanner : Maybe Editing -> Banner -> Html Msg
viewSingleBanner editing data =
    tr []
        [ td [ style <| columnStyle SilentColumn ] [ input [ type_ "checkBox", checked data.isSilent, onCheck (ChangeSilent data.id) ] [], text " Ukryj" ]
        , td [ style <| columnStyle ImageColumn ] [ viewImage data ]
        , td [ style <| columnStyle StartDateColumn ] [ text <| viewDate data.startDate ]
        , td [ style <| columnStyle EndDateColumn ] [ text <| viewDate data.endDate ]
        , td [ style <| columnStyle UrlColumn ] [ viewUrl editing data ]
        , td [ style <| columnStyle WeightColumn ] [ viewWeight editing data ]
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
        , tbody [] (List.map (viewSingleBanner model.editing) model.banners)
        ]

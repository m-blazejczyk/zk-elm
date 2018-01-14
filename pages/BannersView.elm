module BannersView exposing (view)

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


viewInputButtons: Attribute Msg -> Html Msg
viewInputButtons onOkClick = 
    div [ class "btn-group right-align" ]
        [ button [ class "btn btn-default btn-sm", style [ ( "color", "green" ) ], onOkClick ]
            [ glyphicon "ok" NoSpace ]
        , button [ class "btn btn-default btn-sm", style [ ( "color", "red" ) ], onClick CancelEditing ]
            [ glyphicon "remove" NoSpace ]
        ]


viewInputWrapper: Attribute Msg -> Maybe String -> Html Msg -> Html Msg
viewInputWrapper onOkClick hint content =
    let
        hintHtml = 
            case hint of
                Just actualHint ->
                    span [ class "tytul" ] [ text <| "(" ++ actualHint ++ ")" ]
                Nothing ->
                    text ""
            
    in
            
    div [ class "full-width" ]
        [ content, hintHtml, viewInputButtons onOkClick ]


viewRawInput: Int -> String -> Html Msg
viewRawInput maxLen val =
    input [ maxlength maxLen, value val, type_ "text", class "form-control", id "inPlaceEditor", onInput ChangeInput ]
        []


viewInputNormal: Int -> String -> Maybe String -> Attribute Msg -> Html Msg
viewInputNormal maxLen val hint onOkClick =
    viewInputWrapper
        onOkClick
        hint
        (div [ class "form-group full-width-input" ]
            [ viewRawInput maxLen val ])


viewInputWithError: Int -> String -> Maybe String -> Attribute Msg -> Html Msg
viewInputWithError maxLen val hint onOkClick =
    viewInputWrapper
        onOkClick
        hint
        (div [ class "form-group has-error has-feedback full-width-input" ]
            [ viewRawInput maxLen val
            , span [ class "glyphicon glyphicon-exclamation-sign form-control-feedback" ] []
            ])


viewEditingInput: Maybe Editing -> Int -> Html Msg -> Column -> Int -> Maybe String -> Attribute Msg -> Html Msg
viewEditingInput mEditing id nonEditingView column maxLen hint onOkClick =
    case mEditing of
        Just editing ->
            if editing.id == id && editing.column == column then
                if editing.isError then
                    viewInputWithError maxLen editing.value hint onOkClick
                else
                    viewInputNormal maxLen editing.value hint onOkClick
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

    in

        viewEditingInput
            mEditing data.id nonEditingView WeightColumn 2 Nothing
            (onClick (ValidateEditing validateWeight modifyWeight))


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
           
    in
            
        viewEditingInput
            mEditing data.id nonEditingView UrlColumn 500 Nothing
            (onClick (ValidateEditing validateUrl modifyUrl))


viewDate : Maybe Editing -> Maybe SimpleDate -> Column -> Int -> Html Msg
viewDate mEditing mDate column id =
    let
        dateAsString forEditing =
            case mDate of
                Just date ->
                    dateToString date 
                Nothing ->
                    if forEditing then
                        ""
                    else
                        "Brak daty"

        nonEditingView =
            span [ onClick <| StartEditing id column (dateAsString True) ]
                [ text <| dateAsString False ]

    in

        viewEditingInput
            mEditing id nonEditingView column 10 (Just "rrrr-m-d")
            (onClick (ValidateEditing validateDate (modifyDate column)))


viewDeleteButton : Int -> Html Msg
viewDeleteButton id =
    button [ class "btn btn-danger btn-sm", onClick <| DeleteRow id ]
        [ glyphicon "trash" NoSpace ]


viewSingleBanner : Maybe Editing -> Banner -> Html Msg
viewSingleBanner editing data =
    tr []
        [ td [ style <| columnStyle SilentColumn ] [ input [ type_ "checkBox", checked data.isSilent, onCheck (ChangeSilent data.id) ] [], text " Ukryj" ]
        , td [ style <| columnStyle ImageColumn ] [ viewImage data ]
        , td [ style <| columnStyle StartDateColumn ] [ viewDate editing data.startDate StartDateColumn data.id ]
        , td [ style <| columnStyle EndDateColumn ] [ viewDate editing data.endDate EndDateColumn data.id ]
        , td [ style <| columnStyle UrlColumn ] [ viewUrl editing data ]
        , td [ style <| columnStyle WeightColumn ] [ viewWeight editing data ]
        , td [ style <| columnStyle ActionsColumn ] [ viewDeleteButton data.id ]
        ]


view : Model -> Html Msg
view model =
    if List.isEmpty model.banners then
        br [] []
    else
        div [] 
            [ table [ class "table table-bordered" ]
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
            , button [ class "btn btn-primary", onClick AddRow ]
                [ text "Dodaj banner" ]
            ]

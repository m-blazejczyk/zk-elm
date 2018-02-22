module BannersView exposing (view)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)

import Global exposing (..)
import Ui exposing (..)


-- Model, Msg, ...

import Banners exposing (..)


columnTooltip : Column -> Maybe (Html Msg)
columnTooltip column =
    let
        tooltipText =
            case column of
                SilentColumn ->
                    Just "Zaznaczenie pola wyboru w tej kolumnie spowoduje, że dany banner nie będzie wyświetlany."

                StartDateColumn ->
                    Just "Data, od której banner powinien być wyświetlany.  Jeśli jest jej brak, banner będzie wyświetlany od momentu jego utworzenia."

                EndDateColumn ->
                    Just "Data, do której banner powinien być wyświetlany.  Jeśli jest jej brak, banner będzie wyświetlany po wsze czasy."

                UrlColumn ->
                    Just "Adres strony, do której będzie odsyłać obrazek bannera.  Jeśli jest pusty to banner nie będzie linkiem."

                WeightColumn ->
                    Just "Liczba oznaczająca, jak często ten banner ma się pojawiać na stronie.  Domyślnie - 10.  5 oznacza „dwa razy rzadziej niż normalnie”, 20 oznacza „dwa razy częściej niż normalnie”."

                _ ->
                    Nothing
    in
        Maybe.map (\tt -> glyphiconInfo SpaceRight tt) tooltipText


columnHeader : Column -> Html Msg
columnHeader column =
    let
        headerText =            
            case column of
                SilentColumn ->
                    "Ukryj"

                ImageColumn ->
                    "Obrazek"

                StartDateColumn ->
                    "Od…"

                EndDateColumn ->
                    "…do"

                UrlColumn ->
                    "Link"

                WeightColumn ->
                    "Waga"

                ActionsColumn ->
                     ""

    in

        if isColumnSortable column then
                a [ href "#", onClick <| SwitchSort column ] [ text headerText ]
            else
                text headerText


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


columnStyle : Column -> List (Html.Attribute Msg)
columnStyle column = 
    let
        attrs =
            case columnWidth column of
                Just width ->
                    [ ( "width", (toString width) ++ "px" ) ]

                Nothing ->
                    []

    in

        [ style attrs ]


viewImage : Banner -> Html Msg
viewImage data =
    case data.image of
        Just image ->
            div [ style [ ( "text-align", "center" ) ] ]
                [ img [ src ("http://www.zeszytykomiksowe.org/aktualnosci/bannery/" ++ image.file), width image.width, height image.height ] []
                , br [] []
                , span [] [ text (toString image.width ++ " × " ++ toString image.height ++ " px") ]
                ]
        Nothing ->
            text "Brak obrazka"


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
    button [ class "btn btn-danger btn-sm", onClick <| DeleteBannerClick id ]
        [ glyphicon "trash" NoSpace ]


viewSingleBanner : Maybe Editing -> Banner -> Html Msg
viewSingleBanner editing data =
    tr []
        [ td (columnStyle SilentColumn) [ input [ type_ "checkBox", checked data.isSilent, onCheck (ChangeSilent data.id) ] [], text " Ukryj" ]
        , td (columnStyle ImageColumn) [ viewImage data ]
        , td (columnStyle StartDateColumn) [ viewDate editing data.startDate StartDateColumn data.id ]
        , td (columnStyle EndDateColumn) [ viewDate editing data.endDate EndDateColumn data.id ]
        , td (columnStyle UrlColumn) [ viewUrl editing data ]
        , td (columnStyle WeightColumn) [ viewWeight editing data ]
        , td (columnStyle ActionsColumn) [ viewDeleteButton data.id ]
        ]


view : Model -> Html Msg
view model =
    let
        addSortGlyphicon column =
            case model.sortOrder of
                Just (sortColumn, order) ->
                    if sortColumn == column then
                        if order == Ascending then
                            (glyphicon "arrow-up" SpaceRight) :: [ columnHeader column ]
                        else
                            (glyphicon "arrow-down" SpaceRight) :: [ columnHeader column ]
                    else
                        [ columnHeader column ]

                Nothing ->
                    [ columnHeader column ]

        buildHeaderHtml column =
            case columnTooltip column of
                Just tt ->
                    tt :: addSortGlyphicon column

                Nothing ->
                    addSortGlyphicon column

        displayTable =
            table [ class "table table-bordered" ]
                    [ thead []
                        [ tr []
                            [ th (columnStyle SilentColumn)    (buildHeaderHtml SilentColumn)
                            , th (columnStyle ImageColumn)     (buildHeaderHtml ImageColumn)
                            , th (columnStyle StartDateColumn) (buildHeaderHtml StartDateColumn)
                            , th (columnStyle EndDateColumn)   (buildHeaderHtml EndDateColumn)
                            , th (columnStyle UrlColumn)       (buildHeaderHtml UrlColumn)
                            , th (columnStyle WeightColumn)    (buildHeaderHtml WeightColumn)
                            , th (columnStyle ActionsColumn)   (buildHeaderHtml ActionsColumn)
                            ]
                        ]
                    , tbody [] (List.map (viewSingleBanner model.editing) model.banners)
                    ]

    in

        div [] 
            [ viewErrorMsg model.errorMsg CloseErrorMsg
            , viewSpinner model.isLoading
            , if List.isEmpty model.banners then
                text ""
              else
                displayTable
            , button [ class "btn btn-primary", onClick AddBannerClick ]
                [ text "Dodaj banner" ]
            , button [ class "btn btn-primary", style [ ( "margin-left", "20px" ) ], onClick LoadBannersClick ]
                [ glyphicon "refresh" NoSpace ]
            ]

module BannersView exposing (view)

-- Model, Msg, ...

import Banners exposing (..)
import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Ui exposing (..)
import Image
import UiEditing exposing (..)


columnTooltip : Column -> Maybe (Html Msg)
columnTooltip column =
    let
        tooltipText =
            case column of
                SilentColumn ->
                    Just "Zaznaczenie pola wyboru w tej kolumnie spowoduje, że dany banner nie będzie wyświetlany."

                ImageColumn ->
                    Just "Można nagrywać pliki PNG i JPG.  Obrazki zostaną przeskalowane do szerokości 200 pikseli."

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
            Just 250

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
    case columnWidth column of
        Just width ->
            [ style "width" (String.fromInt width ++ "px") ]

        Nothing ->
            []


isEdited : Column -> Int -> Editing -> Bool
isEdited column id editing = 
    editing.id == id && editing.column == column


getUploadStatus : Editing -> Maybe Image.UploadStatus
getUploadStatus editing =
    editing.mUploadStatus


isError : Editing -> Bool
isError editing =
    editing.isError


getValue : Editing -> String
getValue editing =
    editing.value


viewImage : Maybe Editing -> Banner -> Html Msg
viewImage mEditing banner =
    let
        fakeValidator : String -> Maybe String
        fakeValidator _ =
            Nothing

        fakeModifier : String -> Banner -> Banner
        fakeModifier _ b =
            b
        
    in
    viewEditableImage
        mEditing
        (isEdited ImageColumn banner.id)
        getUploadStatus
        isError
        getValue
        banner.mImage
        { mHint = Nothing
        , maxLen = 0
        , changeMsg = ChangeInput
        , cancelMsg = CancelEditing
        , submitMsg = SubmitFileUpload fakeValidator fakeModifier
        , closeUploadErrorMsg = CloseUploadErrorMsg }
        (StartEditing banner.id ImageColumn "")


viewWeight : Maybe Editing -> Banner -> Html Msg
viewWeight mEditing banner =
    let
        weightAsString =
            String.fromInt banner.weight

        nonEditingView =
            span [ onClick <| StartEditing banner.id WeightColumn weightAsString ]
                [ text weightAsString ]
    in
    viewEditableField
        mEditing
        (isEdited WeightColumn banner.id)
        getUploadStatus
        isError
        getValue
        nonEditingView
        textEditorView
        { mHint = Nothing
        , maxLen = 2
        , changeMsg = ChangeInput
        , cancelMsg = CancelEditing
        , submitMsg = ValidateEditing validateWeight modifyWeight
        , closeUploadErrorMsg = CloseUploadErrorMsg }


shorterUrl : Maybe String -> String
shorterUrl mUrl =
    case mUrl of
        Nothing ->
            "Brak linka"

        Just url ->
            let
                urlNoHttp =
                    if String.startsWith "http://" url then
                        String.dropLeft 7 url

                    else if String.startsWith "https://" url then
                        String.dropLeft 8 url

                    else
                        url
            in
            if String.length urlNoHttp < 20 then
                urlNoHttp

            else
                String.left 20 urlNoHttp ++ "…"


viewUrl : Maybe Editing -> Banner -> Html Msg
viewUrl mEditing banner =
    let
        nonEditingView =
            span
                [ class "with-tooltip"
                , onClick <| StartEditing banner.id UrlColumn (Maybe.withDefault "" banner.mUrl)
                ]
                [ text <| shorterUrl banner.mUrl
                , span [ class "tooltip-text tooltip-span" ]
                    [ text (Maybe.withDefault "" banner.mUrl) ]
                ]
    in
    viewEditableField
        mEditing
        (isEdited UrlColumn banner.id)
        getUploadStatus
        isError
        getValue
        nonEditingView
        textEditorView
        { mHint = Nothing
        , maxLen = 500
        , changeMsg = ChangeInput
        , cancelMsg = CancelEditing
        , submitMsg = ValidateEditing validateUrl modifyUrl
        , closeUploadErrorMsg = CloseUploadErrorMsg }


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
    viewEditableField
        mEditing
        (isEdited column id)
        getUploadStatus
        isError
        getValue
        nonEditingView
        textEditorView
        { mHint = Just "rrrr-m-d"
        , maxLen = 10
        , changeMsg = ChangeInput
        , cancelMsg = CancelEditing
        , submitMsg = ValidateEditing validateDate (modifyDate column)
        , closeUploadErrorMsg = CloseUploadErrorMsg }


viewDeleteButton : Int -> Html Msg
viewDeleteButton id =
    button [ class "btn btn-danger btn-sm", onClick <| DeleteBannerClick id ]
        [ glyphicon "trash" NoSpace ]


viewSingleBanner : Maybe Editing -> Banner -> Html Msg
viewSingleBanner editing banner =
    tr []
        [ td (columnStyle SilentColumn) [ input [ type_ "checkBox", checked banner.isSilent, onCheck (ChangeSilent banner.id) ] [], text " Ukryj" ]
        , td (columnStyle ImageColumn) [ viewImage editing banner ]
        , td (columnStyle StartDateColumn) [ viewDate editing banner.mStartDate StartDateColumn banner.id ]
        , td (columnStyle EndDateColumn) [ viewDate editing banner.mEndDate EndDateColumn banner.id ]
        , td (columnStyle UrlColumn) [ viewUrl editing banner ]
        , td (columnStyle WeightColumn) [ viewWeight editing banner ]
        , td (columnStyle ActionsColumn) [ viewDeleteButton banner.id ]
        ]


view : Model -> Html Msg
view model =
    let
        addSortGlyphicon column =
            case model.sortOrder of
                Just ( sortColumn, order ) ->
                    if sortColumn == column then
                        if order == Ascending then
                            [ glyphicon "arrow-up" SpaceRight, columnHeader column ]

                        else
                            [ glyphicon "arrow-down" SpaceRight, columnHeader column ]

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
                        [ th (columnStyle SilentColumn) (buildHeaderHtml SilentColumn)
                        , th (columnStyle ImageColumn) (buildHeaderHtml ImageColumn)
                        , th (columnStyle StartDateColumn) (buildHeaderHtml StartDateColumn)
                        , th (columnStyle EndDateColumn) (buildHeaderHtml EndDateColumn)
                        , th (columnStyle UrlColumn) (buildHeaderHtml UrlColumn)
                        , th (columnStyle WeightColumn) (buildHeaderHtml WeightColumn)
                        , th (columnStyle ActionsColumn) (buildHeaderHtml ActionsColumn)
                        ]
                    ]
                , tbody [] (List.map (viewSingleBanner model.editing) model.banners)
                ]
    in
    div []
        [ viewErrorMsg model.errorMsg CloseErrorMsg
        , viewSpinner model.isLoading
        , button [ class "btn btn-primary", onClick AddBannerClick ]
            [ text "Dodaj banner" ]
        , button [ class "btn btn-primary", style "margin-left" "20px", onClick LoadBannersClick ]
            [ glyphicon "refresh" NoSpace ]
        , if List.isEmpty model.banners then
            text ""

          else
            displayTable
        ]

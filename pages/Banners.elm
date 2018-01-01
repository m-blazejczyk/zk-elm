module Banners exposing (Msg(..), Banner, Editing, Model, Column(..), init, update)

import Date exposing (Date)
import Result


type Column
    = SilentColumn
    | ImageColumn
    | StartDateColumn
    | EndDateColumn
    | UrlColumn
    | WeightColumn
    | ActionsColumn


type Msg
    = ChangeSilent Int Bool
    | StartEditing Int Column String
    | FocusResult (Result Dom.Error ())


type alias Banner =
    { id : Int
    , isSilent : Bool
    , startDate : Maybe Date
    , endDate : Maybe Date
    , image : String
    , imageH : Int
    , imageW : Int
    , url : String
    , weight : Int
    }


type alias Editing =
    { id : Int
    , column : Column
    , value : String
    , isError : Bool
    }


type alias Model =
    { banners : List Banner
    , editing : Maybe Editing
    }


init : Model
init =
    Model
        [ Banner 1 False Nothing (Result.toMaybe <| Date.fromString "2018/1/15") "http://www.zeszytykomiksowe.org/aktualnosci/bannery/dydaktyczny-potencjal.jpg" 89 200 "http://fundacja-ikp.pl/wydawnictwo/" 10
        , Banner 2 True (Result.toMaybe <| Date.fromString "2018/1/1") Nothing "http://www.zeszytykomiksowe.org/aktualnosci/bannery/dydaktyczny-potencjal.jpg" 89 200 "http://www.cbc.ca/news/canada/montreal/montreal-together-spaces-reconciliation-1.4117290" 20
        ]
        Nothing


updateSilent : Int -> Bool -> Banner -> Banner
updateSilent id checked banner =
    if id == banner.id then
        { banner | isSilent = checked }
    else
        banner


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ChangeSilent id checked ->
            ( { model | banners = List.map (updateSilent id checked) model.banners }, Cmd.none )

        StartEditing id column value ->
            ( { model | editing = Just (Editing id column value False) }
            , Dom.focus "inPlaceEditor" |> Task.attempt FocusResult )

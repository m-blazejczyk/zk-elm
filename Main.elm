port module ZKMain exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Global exposing (..)
import Page exposing (..)
import Model exposing (..)
import Msg exposing (..)
import ViewTemplate exposing (..)
import Banners


main : Program (Maybe ModelForPorts) Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = view
        }



-- Initialization


init : Maybe ModelForPorts -> ( Model, Cmd Msg )
init mModelFP =
    case mModelFP of
        Just modelFP ->
            ( convertModelFromPort modelFP, Cmd.none )

        Nothing ->
            ( Model "" MainMenu emptyUser Banners.init, Cmd.none )



{-
   UPDATE
   * API routes
   * GET and POST
   * Encode request body
   * Decode responses
   * Messages
   * Ports
   * Update case
-}
-- POST register / login request


authUserReqFormBody : Model -> Http.Body
authUserReqFormBody model =
    Http.multipartBody
        [ Http.stringPart "user" model.user.userName
        , Http.stringPart "password" model.user.password
        ]


authUserCmd : Model -> Cmd Msg
authUserCmd model =
    Http.send GetTokenCompleted <| Http.post (domain ++ "auth/login") (authUserReqFormBody model) userDecoder


getTokenCompleted : Model -> Result Http.Error User -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok newUser ->
            setStorageHelper { model | page = MainMenu, user = newUser, errorMsg = "" }

        Err (Http.BadStatus response) ->
            if response.status.code == 401 then
                ( { model | errorMsg = "Niewłaściwy użytkownik albo hasło" }, Cmd.none )
            else
                ( { model | errorMsg = "Błąd " ++ (toString response.status.code) }, Cmd.none )

        Err error ->
            ( { model | errorMsg = (toString error) }, Cmd.none )



-- GET request for random protected quote (authenticated)


fetchProtectedQuote : Model -> Http.Request String
fetchProtectedQuote model =
    { method = "GET"
    , headers = [ Http.header "Authorization" ("Token " ++ model.user.token) ]
    , url = ""
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }
        |> Http.request



-- Ports


port setStorage : ModelForPorts -> Cmd msg


port removeStorage : () -> Cmd msg



-- Helper to update model and set local storage with the updated model


setStorageHelper : Model -> ( Model, Cmd Msg )
setStorageHelper model =
    ( model, setStorage <| convertModelForPort model )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickLogIn ->
            ( model, authUserCmd model )

        SetUsername username ->
            ( setUserNameInModel username model, Cmd.none )

        SetPassword password ->
            ( setPasswordInModel password model, Cmd.none )

        GetTokenCompleted result ->
            getTokenCompleted model result

        LogOut ->
            ( { model | page = MainMenu, user = emptyUser }, removeStorage () )

        OpenPage page ->
            ( { model | page = page }, Cmd.none )

        BannersMsg innerMsg ->
            let
                ( innerModel, innerCmd ) =
                    Banners.update innerMsg model.banners
            in
                ( { model | banners = innerModel }, Cmd.map BannersMsg innerCmd )


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ viewHeader <| loggedInUser model
        , viewTopMenu
        , div [ id "article" ]
            [ viewError model.errorMsg
            , viewTitle model
            , viewContent model
            ]
        , viewFooter
        ]

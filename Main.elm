port module ZKMain exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Global exposing (..)
import Page exposing (..)
import Model exposing (..)
import Ui exposing (viewErrorMsg)
import ViewTemplate exposing (..)
import Banners
import BannersView


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
            let
                model = convertModelFromPort modelFP

            in

                ( model, openPageCmd model.page )

        Nothing ->
            ( Model Nothing MainMenu emptyUser Banners.init, Cmd.none )


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


getTokenCompleted : Model -> Result Http.Error User -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok newUser ->
            setStorageHelper { model | page = MainMenu, user = newUser, loginErrorMsg = Nothing }

        Err (Http.BadStatus response) ->
            if response.status.code == 401 then
                ( { model | loginErrorMsg = Just "Niewłaściwy użytkownik albo hasło" }, Cmd.none )
            else
                ( { model | loginErrorMsg = Just <| "Błąd " ++ (toString response.status.code) }, Cmd.none )

        Err error ->
            ( { model | loginErrorMsg = Just <| toString error }, Cmd.none )


-- Ports


port setStorage : ModelForPorts -> Cmd msg


port removeStorage : () -> Cmd msg


-- Helper to update model and set local storage with the updated model


setStorageHelper : Model -> ( Model, Cmd Msg )
setStorageHelper model =
    ( model, setStorage <| convertModelForPort model )


openPageCmd : Page -> Cmd Msg
openPageCmd page =
    case page of
        Banners ->
            Cmd.map BannersMsg Banners.switchToPageCmd

        _ ->
            Cmd.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickLogIn ->
            ( model, Http.send GetTokenCompleted <| Http.post (domain ++ "auth/login") (authUserReqFormBody model) userDecoder )

        SetUsername username ->
            ( setUserNameInModel username model, Cmd.none )

        SetPassword password ->
            ( setPasswordInModel password model, Cmd.none )

        GetTokenCompleted result ->
            getTokenCompleted model result

        LogOut ->
            ( { model | page = MainMenu, user = emptyUser }, removeStorage () )

        CloseErrorMsg ->
            ( { model | loginErrorMsg = Nothing }, Cmd.none )

        OpenPage page ->
            ( { model | page = page }, openPageCmd page )

        BannersMsg innerMsg ->
            let
                ( innerModel, innerCmd ) =
                    Banners.update innerMsg model.banners

            in
                    
                ( { model | banners = innerModel }
                , Cmd.map BannersMsg innerCmd )


viewPage : Model -> Html Msg
viewPage model =
    let
        noContent =
            p [ class "text-center" ] [ text "Ta strona jeszcze nie istnieje." ]
    in
        case model.page of
            MainMenu ->
                viewMainMenu

            Banners ->
                BannersView.view model.banners |> Html.map BannersMsg

            _ ->
                noContent


view : Model -> Html Msg
view model =
    div [ id "container" ]
        [ viewHeader <| loggedInUser model
        , viewTopMenu
        , div [ id "article" ]
            [ viewErrorMsg model.loginErrorMsg CloseErrorMsg
            , viewTitle model
            , if isLoggedIn model then
                viewPage model
              else
                viewLoginForm model.user
            ]
        , viewFooter
        ]

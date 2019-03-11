port module ZKMain exposing (main)

import Banners
import BannersView
import Issues
import IssuesView
import Global exposing (..)
import Paths
import Browser exposing (Document, document)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as E
import Model exposing (..)
import Page exposing (..)
import Ui exposing (viewErrorMsg)
import ViewTemplate exposing (..)


main : Program (Maybe ModelForPorts) Model Msg
main =
    Browser.document
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- Initialization


init : Maybe ModelForPorts -> ( Model, Cmd Msg )
init mModelFP =
    case mModelFP of
        Just modelFP ->
            let
                model =
                    modelFromPort modelFP
            in
            ( model, openPageCmd model.page )

        Nothing ->
            ( Model "" "" Nothing MainMenu Nothing Banners.init Issues.init, Cmd.none )



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
        [ Http.stringPart "user" model.loginUserName
        , Http.stringPart "password" model.loginPassword
        ]


getTokenCompleted : Model -> Result Http.Error User -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok newUser ->
            ( { model | page = MainMenu, mUser = Just newUser,
                loginErrorMsg = Nothing, loginUserName = "", loginPassword = "" }
            , setStorage <| ModelForPorts (Page.toString MainMenu) newUser )

        Err (Http.BadStatus response) ->
            if response.status.code == 401 then
                ( { model | mUser = Nothing, loginErrorMsg = Just "Niewłaściwy użytkownik albo hasło" }
                , Cmd.none )

            else
                ( { model | mUser = Nothing, loginErrorMsg = Just <| "Błąd " ++ String.fromInt response.status.code }
                , Cmd.none )

        Err error ->
            ( { model | mUser = Nothing, loginErrorMsg = Just <| httpErrToString error }
            , Cmd.none )



-- Ports


port setStorage : ModelForPorts -> Cmd msg


port removeStorage : () -> Cmd msg


openPageCmd : Page -> Cmd Msg
openPageCmd page =
    case page of
        Banners ->
            Cmd.map BannersMsg Banners.switchToPageCmd

        _ ->
            Cmd.none


setStorageCmd : Page -> Maybe User -> Cmd Msg
setStorageCmd page mUser =
    case mUser of
        Just user ->
            setStorage <| ModelForPorts (Page.toString page) user

        Nothing ->
            Cmd.none


subscriptions : Model -> Sub Msg
subscriptions model =
    case model.page of
        Page.Banners ->
            Sub.map BannersMsg (Banners.fileUploadStatus Banners.FileUploadStatus)

        -- Should never happen!
        _ ->
            Sub.none


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        ClickLogIn ->
            let
                loginUrl = Paths.api [ "auth", "login" ]
                    
            in
                ( model, Http.send GetTokenCompleted <| Http.post loginUrl (authUserReqFormBody model) userDecoder )

        SetUsername username ->
            ( { model | loginUserName = username }, Cmd.none )

        SetPassword password ->
            ( { model | loginPassword = password }, Cmd.none )

        GetTokenCompleted result ->
            getTokenCompleted model result

        LogOut ->
            ( { model | page = MainMenu, mUser = Nothing }, removeStorage () )

        CloseErrorMsg ->
            ( { model | loginErrorMsg = Nothing }, Cmd.none )

        OpenPage page ->
            ( { model | page = page }
            , Cmd.batch [ openPageCmd page, setStorageCmd page model.mUser ] )

        BannersMsg innerMsg ->
            case model.mUser of
                Just user ->
                    let
                        ( innerModel, innerCmd ) =
                            Banners.update innerMsg model.banners user.token
                    in
                    ( { model | banners = innerModel }
                    , Cmd.map BannersMsg innerCmd
                    )

                Nothing ->
                    ( model, Cmd.none )

        IssuesMsg innerMsg ->
            case model.mUser of
                Just user ->
                    let
                        ( innerModel, innerCmd ) =
                            Issues.update innerMsg model.issues user.token
                    in
                    ( { model | issues = innerModel }
                    , Cmd.map IssuesMsg innerCmd
                    )

                Nothing ->
                    ( model, Cmd.none )


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

        Issues ->
            IssuesView.view model.issues |> Html.map IssuesMsg

        _ ->
            noContent


view : Model -> Document Msg
view model =
    { title = "Portal redakcyjny „Zeszytów komiksowych”"
    , body = 
        [ div
            [ id "container" ]
            [ viewHeader model.mUser
            , viewTopMenu
            , div [ id "article" ]
                [ viewErrorMsg model.loginErrorMsg CloseErrorMsg
                , viewTitle model
                , if maybeIsJust model.mUser then
                    viewPage model

                  else
                    viewLoginForm model.loginUserName model.loginPassword
                ]
            , viewFooter
            ]
        ] }

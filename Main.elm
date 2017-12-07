port module ZKMain exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)


main : Program (Maybe Model) Model Msg
main =
    Html.programWithFlags
        { init = init
        , update = update
        , subscriptions = \_ -> Sub.none
        , view = viewTemplate
        }



{-
    HELPER FUNCTIONS
-}



domain : String
domain = "https://red.zeszytykomiksowe.org/"

{-
   MODEL
   * Model type
   * Initialize model with empty values
-}


type alias User =
    { userId : Int
    , userName : String
    , password : String
    , fullName : String
    , initials : String
    }


type alias Model =
    { token : String
    , errorMsg : String
    , user : User
    }


init : Maybe Model -> ( Model, Cmd Msg )
init model =
    case model of
        Just model ->
            ( model, Cmd.none )

        Nothing ->
            ( Model "" "" (User 0 "" "" "" ""), Cmd.none )



-- Setters for nested data within the model


setUserNameInModel : String -> Model -> Model
setUserNameInModel username model =
    let
        oldUser =
            model.user

        newUser =
            { oldUser | userName = username }
    in
        { model | user = newUser }


setPasswordInModel : String -> Model -> Model
setPasswordInModel password model =
    let
        oldUser =
            model.user

        newUser =
            { oldUser | password = password }
    in
        { model | user = newUser }


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
    Http.send GetTokenCompleted <| Http.post (domain ++ "auth/login") (authUserReqFormBody model) tokenDecoder


getTokenCompleted : Model -> Result Http.Error String -> ( Model, Cmd Msg )
getTokenCompleted model result =
    case result of
        Ok newToken ->
            -- https://medium.com/elm-shorts/updating-nested-records-in-elm-15d162e80480
            let
                newModel =
                    setPasswordInModel "" model
            in
                setStorageHelper { newModel | token = newToken, errorMsg = "" }

        Err (Http.BadStatus response) ->
            if response.status.code == 401 then
                ( { model | errorMsg = "Niewłaściwy użytkownik albo hasło" }, Cmd.none )
            else
                ( { model | errorMsg = "Błąd " ++ (toString response.status.code) }, Cmd.none )

        Err error ->
            ( { model | errorMsg = (toString error) }, Cmd.none )



-- Decode POST response to get access token and user information


tokenDecoder : Decoder String
tokenDecoder =
    Decode.field "token" Decode.string



-- GET request for random protected quote (authenticated)


fetchProtectedQuote : Model -> Http.Request String
fetchProtectedQuote model =
    { method = "GET"
    , headers = [ Http.header "Authorization" ("Token " ++ model.token) ]
    , url = ""
    , body = Http.emptyBody
    , expect = Http.expectString
    , timeout = Nothing
    , withCredentials = False
    }
        |> Http.request



-- Helper to update model and set local storage with the updated model


setStorageHelper : Model -> ( Model, Cmd Msg )
setStorageHelper model =
    ( model, setStorage model )



-- Messages


type Msg
    = ClickLogIn
    | SetUsername String
    | SetPassword String
    | GetTokenCompleted (Result Http.Error String)
    | LogOut



-- Ports


port setStorage : Model -> Cmd msg


port removeStorage : Model -> Cmd msg



-- Update


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
            let
                newModel =
                    setUserNameInModel "" model
            in
                ( { newModel | token = "" }, removeStorage model )



{-
   VIEW
   * Hide sections of view depending on authenticaton state of model
   * Get a quote
   * Log In or Register
   * Get a protected quote
-}


viewTemplate : Model -> Html Msg
viewTemplate model = 
    div [ id "container" ]
        [ div [ attribute "style" "height: 95px;" ]
            [ div [ id "logo-zk" ]
                [ img [ attribute "border" "0", attribute "height" "150", src (domain ++ "static/ZK_logo_red.png"), attribute "width" "150" ]
                    []
                ]
            , div [ id "logo-centrala", attribute "style" "align: right;" ]
                [ text "Użytkownik" ]
            ]
        , div [ id "nav-menu" ]
            [ ul []
                [ li []
                    [ a [ href "/", title "Niusy" ]
                        [ text "niusy" ]
                    ]
                , li []
                    [ a [ href "/", title "Numery" ]
                        [ text "numery" ]
                    ]
                , li []
                    [ a [ href "/", title "Recenzje" ]
                        [ text "recenzje" ]
                    ]
                , li [ attribute "style" "width: 150px;" ]
                    []
                , li []
                    [ a [ href "/", title "Składnica" ]
                        [ text "składnica" ]
                    ]
                , li []
                    [ a [ href "/", title "Bannery" ]
                        [ text "bannery" ]
                    ]
                , li []
                    [ a [ href "/", title "E-publikacje" ]
                        [ text "e-publikacje" ]
                    ]
                ]
            ]
        , div [ id "article" ]
            [ view model ]
        , div [ id "footer" ]
            [ div [ id "footer-slogan" ]
                [ text "O komiksie. Na serio." ]
            , div [ id "footer-copy" ]
                [ text "layout © Michał Błażejczyk 2012 || logo © "
                , a [ href "mailto:dennis.wojda@gmail.com" ]
                    [ text " Dennis Wojda" ]
                , text "2011"
                ]
            ]
        ]


view : Model -> Html Msg
view model =
    let
        -- Is the user logged in?
        loggedIn : Bool
        loggedIn =
            String.length model.token > 0

        -- If the user is logged in, show a greeting; if logged out, show the login/register form
        authBoxView =
            let
                -- If there is an error on authentication, show the error alert
                showError : String
                showError =
                    if String.isEmpty model.errorMsg then
                        "hidden"
                    else
                        ""

                -- Greet a logged in user by username
                greeting : String
                greeting =
                    "Hello, " ++ model.user.userName ++ "!"
            in
                if loggedIn then
                    div [ id "greeting" ]
                        [ h3 [ class "text-center" ] [ text greeting ]
                        , p [ class "text-center" ] [ text "Login był udany!" ]
                        , p [ class "text-center" ]
                            [ button [ class "btn btn-danger", onClick LogOut ] [ text "Wyloguj się" ]
                            ]
                        ]
                else
                    div [ id "form" ]
                        [ h2 [ class "text-center" ] [ text "Proszę się zalogować" ]
                        , div [ class showError ]
                            [ div [ class "alert alert-danger" ] [ text model.errorMsg ]
                            ]
                        , div [ class "form-group row" ]
                            [ div [ class "col-md-offset-2 col-md-8" ]
                                [ label [ for "username" ] [ text "Username:" ]
                                , input [ id "username", type_ "text", class "form-control", Html.Attributes.value model.user.userName, onInput SetUsername ] []
                                ]
                            ]
                        , div [ class "form-group row" ]
                            [ div [ class "col-md-offset-2 col-md-8" ]
                                [ label [ for "password" ] [ text "Password:" ]
                                , input [ id "password", type_ "password", class "form-control", Html.Attributes.value model.user.password, onInput SetPassword ] []
                                ]
                            ]
                        , div [ class "text-center" ]
                            [ button [ class "btn btn-primary", onClick ClickLogIn ] [ text "Log In" ]
                            ]
                        ]
    in
        div [ class "container" ]
            [ h2 [ class "text-center" ] [ text "Portal redakcyjny „Zeszytów Komiksowych”" ]
            , div [ class "jumbotron text-left" ]
                [ -- Login/Register form or user greeting
                  authBoxView
                ]
            ]

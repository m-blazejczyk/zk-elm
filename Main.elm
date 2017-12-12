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
        , view = view
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
    { token : String
    , userId : Int
    , userName : String
    , password : String
    , fullName : String
    , initials : String
    }


emptyUser : User
emptyUser = 
    User "" 0 "" "" "" ""


type alias Model =
    { errorMsg : String
    , user : User
    }


init : Maybe Model -> ( Model, Cmd Msg )
init model =
    case model of
        Just model ->
            ( model, Cmd.none )

        Nothing ->
            ( Model "" emptyUser, Cmd.none )



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


-- Is the user logged in?
isLoggedIn : Model -> Bool
isLoggedIn model =
    String.length model.user.token > 0


loggedInUser : Model -> Maybe User
loggedInUser model = 
    if isLoggedIn model then
        Just model.user
    else
        Nothing


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
            setStorageHelper { model | user = newUser, errorMsg = "" }

        Err (Http.BadStatus response) ->
            if response.status.code == 401 then
                ( { model | errorMsg = "Niewłaściwy użytkownik albo hasło" }, Cmd.none )
            else
                ( { model | errorMsg = "Błąd " ++ (toString response.status.code) }, Cmd.none )

        Err error ->
            ( { model | errorMsg = (toString error) }, Cmd.none )



-- Decode POST response to get access token and user information


userDecoder : Decoder User
userDecoder =
    let
        makeUser token userId userName fullName initials =
            User token userId userName "" fullName initials

    in

        Decode.map5
            makeUser
            (Decode.field "token" Decode.string)
            (Decode.field "userId" Decode.int)
            (Decode.field "userName" Decode.string)
            (Decode.field "fullName" Decode.string)
            (Decode.field "initials" Decode.string)



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



-- Helper to update model and set local storage with the updated model


setStorageHelper : Model -> ( Model, Cmd Msg )
setStorageHelper model =
    ( model, setStorage model )



-- Messages


type Msg
    = ClickLogIn
    | SetUsername String
    | SetPassword String
    | GetTokenCompleted (Result Http.Error User)
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
            ( { model | user = emptyUser }, removeStorage model )



{-
   VIEW
-}


viewUserMenu : Maybe User -> List (Html Msg)
viewUserMenu mUser =
    case mUser of
        Just user ->
            [ b [] [ text user.fullName ]
            , button [ class "btn btn-danger btn-xs", onClick LogOut ] [ text "Wyloguj się" ]
            ]
        Nothing ->
            [ text "" ]

viewHeader : Maybe User -> Html Msg
viewHeader mUser = 
    div [ attribute "style" "height: 95px;" ]
        [ div [ id "logo-zk" ]
            [ img [ src (domain ++ "static/ZK_logo_red.png") ]
                []
            ]
        , div [ id "user-menu" ]
            (viewUserMenu mUser)
        ]


viewMenu : Html Msg
viewMenu = 
    div [ id "nav-menu" ]
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


viewFooter : Html Msg
viewFooter = 
    div [ id "footer" ]
        [ div [ id "footer-slogan" ]
            [ text "O komiksie. Na serio." ]
        , div [ id "footer-copy" ]
            [ text "layout © Michał Błażejczyk 2012 || logo © "
            , a [ href "mailto:dennis.wojda@gmail.com" ]
                [ text " Dennis Wojda" ]
            , text "2011"
            ]
        ]


viewLoginForm : User -> Html Msg
viewLoginForm user =
    div [ id "form" ]
        [ div [ class "form-group row" ]
            [ div [ class "col-md-offset-2 col-md-8" ]
                [ label [ for "username" ] [ text "Kto?" ]
                , input [ id "username", type_ "text", class "form-control", Html.Attributes.value user.userName, onInput SetUsername ] []
                ]
            ]
        , div [ class "form-group row" ]
            [ div [ class "col-md-offset-2 col-md-8" ]
                [ label [ for "password" ] [ text "Hasło:" ]
                , input [ id "password", type_ "password", class "form-control", Html.Attributes.value user.password, onInput SetPassword ] []
                ]
            ]
        , div [ class "text-center" ]
            [ button [ class "btn btn-primary", onClick ClickLogIn ] [ text "Zaloguj się" ]
            ]
        ]


viewContent : Model -> List (Html Msg)
viewContent model =
    let
        -- If there is an error on authentication, show the error alert
        showErrorClass : String
        showErrorClass =
            if String.isEmpty model.errorMsg then
                "hidden"
            else
                ""

        authBoxView =
            if isLoggedIn model then
                div [ id "greeting" ]
                    [ p [ class "text-center" ] [ text "Login był udany!" ]
                    , p [ class "text-center" ]
                        [ button [ class "btn btn-danger", onClick LogOut ] [ text "Wyloguj się" ]
                        ]
                    ]
            else
                viewLoginForm model.user
    in
        [ div [ class showErrorClass ]
            [ div [ class "alert alert-danger" ]
                [ text model.errorMsg ]
            ]
        , h2 [ id "title" ]
            [ text "Portal redakcyjny „Zeszytów Komiksowych”" ]
        , div [ class "jumbotron text-left" ]
            [ authBoxView ]
        ]


view : Model -> Html Msg
view model = 
    div [ id "container" ]
        [ viewHeader <| loggedInUser model
        , viewMenu
        , div [ id "article" ]
            ( viewContent model )
        , viewFooter
        ]

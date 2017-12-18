port module ZKMain exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as Encode exposing (..)


main : Program (Maybe ModelForPorts) Model Msg
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
   PAGE
-}


type Page = MainMenu | News | Issues | Reviews | Repository | Banners | HomePageContent


pageTitles : Page -> (String, String)
pageTitles page =
    case page of
        MainMenu ->        ("portal",        "Portal redakcyjny „Zeszytów Komiksowych”")
        News ->            ("newsy",         "Newsy")
        Issues ->          ("numery",        "Numery „Zeszytów Komiksowych”")
        Reviews ->         ("recenzje",      "Recenzje")
        Repository ->      ("składnica",     "Składnica naukowa")
        Banners ->         ("bannery",       "Bannery")
        HomePageContent -> ("strona główna", "Zawartość strony głównej")


editingPages : List Page
editingPages = [ Banners, HomePageContent, News, Issues, Reviews, Repository ]


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
    , page : Page
    , user : User
    }


-- Special type used to save/restore state via ports and flags;
-- Cannot use union types, and there's no need to keep the error message


type alias ModelForPorts =
    { pageStr : String
    , user : User
    }


convertModelForPort : Model -> ModelForPorts
convertModelForPort model =
    ModelForPorts ( toString model.errorMsg ) model.user


convertModelFromPort : ModelForPorts -> Model
convertModelFromPort modelFP =
    let
        page = case modelFP.pageStr of
            "News" ->            News
            "Issues" ->          Issues
            "Reviews" ->         Reviews
            "Repository" ->      Repository
            "Banners" ->         Banners
            "HomePageContent" -> HomePageContent
            _ ->                 MainMenu

    in
        Model "" page modelFP.user



-- Initialization


init : Maybe ModelForPorts -> ( Model, Cmd Msg )
init mModelFP =
    case mModelFP of
        Just modelFP ->
            ( convertModelFromPort modelFP, Cmd.none )

        Nothing ->
            ( Model "" MainMenu emptyUser, Cmd.none )



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
            setStorageHelper { model | page = MainMenu, user = newUser, errorMsg = "" }

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



-- Messages


type Msg
    = ClickLogIn
    | SetUsername String
    | SetPassword String
    | GetTokenCompleted (Result Http.Error User)
    | LogOut



-- Ports


port setStorage : ModelForPorts -> Cmd msg


port removeStorage : () -> Cmd msg



-- Helper to update model and set local storage with the updated model


setStorageHelper : Model -> ( Model, Cmd Msg )
setStorageHelper model =
    ( model, setStorage <| convertModelForPort model )



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
            ( { model | page = MainMenu, user = emptyUser }, removeStorage () )



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
            [ img [ src ( domain ++ "static/ZK_logo_red.png" ) ]
                []
            ]
        , div [ id "user-menu" ]
            ( viewUserMenu mUser )
        ]


viewTopMenu : Html Msg
viewTopMenu = 

    let
        midIndex = ( ( List.length editingPages ) // 2 ) - 1

        buildLi page = 
            let
                ( short, long ) = pageTitles page
            in
                li [] [ a [ href "/", title long ] [ text short ] ]

        mapper page t = 
            let
                ( index, newList ) = t
            in
                if index == midIndex then
                    ( index + 1, ( li [ attribute "style" "width: 150px;" ] [] ) :: ( buildLi page ) :: newList )
                else
                    ( index + 1, ( buildLi page ) :: newList )

        ( _, lis ) = List.foldr mapper ( 0, [] ) editingPages
            
    in
        div [ id "nav-menu" ]
            [ ul [] lis ]


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
    div [ class "jumbotron text-left" ]
        [ div [ id "form" ]
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
        ]


viewError : String -> Html Msg
viewError errorMsg = 
    let
        errorClass : String
        errorClass =
            if String.isEmpty errorMsg then
                "hidden"
            else
                ""
    in
        div [ class errorClass ]
            [ div [ class "alert alert-danger" ]
                [ text errorMsg ]
            ]


viewTitle : Model -> Html Msg
viewTitle model =
    let
        page = 
            if isLoggedIn model then
                model.page
            else
                MainMenu
        ( _, title ) = pageTitles page
    in
        h2 [ id "title" ] [ text title ]


viewPage : Model -> Html Msg
viewPage _ =
    p [ class "text-center" ] [ text "Przykro mi – ta strona jeszcze nie istnieje" ]


viewContent : Model -> Html Msg
viewContent model =
    if isLoggedIn model then
        viewPage model
    else
        viewLoginForm model.user


view : Model -> Html Msg
view model = 
    div [ id "container" ]
        [ viewHeader <| loggedInUser model
        , viewTopMenu
        , div [ id "article" ]
            [ viewError model.errorMsg
            , viewTitle model
            , viewContent model ]
        , viewFooter
        ]

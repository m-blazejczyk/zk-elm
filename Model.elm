module Model exposing (..)

import Http
import Json.Decode as Decode exposing (..)
import Task
import Time exposing (Time)
import Page exposing (..)
import Banners

type Msg
    = ClickLogIn
    | SetUsername String
    | SetPassword String
    | GetTokenCompleted (Result Http.Error User)
    | LogOut
    | OpenPage Page
    | BannersMsg Banners.Msg


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
    { loginErrorMsg : Maybe String
    , page : Page
    , user : User
    , banners : Banners.Model
    }


-- Special type used to save/restore state via ports and flags;
-- Cannot use union types, and there's no need to keep the error message


type alias ModelForPorts =
    { pageStr : String
    , user : User
    }


convertModelForPort : Model -> ModelForPorts
convertModelForPort model =
    ModelForPorts (toString model.page) model.user


convertModelFromPort : ModelForPorts -> Model
convertModelFromPort modelFP =
    let
        page : Page
        page =
            case modelFP.pageStr of
                "News" ->
                    News

                "Issues" ->
                    Issues

                "Reviews" ->
                    Reviews

                "Repository" ->
                    Repository

                "Banners" ->
                    Banners

                "HomePageContent" ->
                    HomePageContent

                _ ->
                    MainMenu

    in

        Model Nothing page modelFP.user Banners.init


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


isLoggedIn : Model -> Bool
isLoggedIn model =
    String.length model.user.token > 0


loggedInUser : Model -> Maybe User
loggedInUser model =
    if isLoggedIn model then
        Just model.user
    else
        Nothing


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

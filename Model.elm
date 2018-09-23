module Model exposing (
    Model, ModelForPorts, Msg(..), User, convertModelForPort, convertModelFromPort, userDecoder)

import Banners
import Http
import Json.Decode as Decode exposing (..)
import Page exposing (..)
import Task


type Msg
    = ClickLogIn
    | SetUsername String
    | SetPassword String
    | GetTokenCompleted (Result Http.Error User)
    | LogOut
    | CloseErrorMsg
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


type alias Model =
    { loginUserName : String
    , loginPassword : String
    , loginErrorMsg : Maybe String
    , page : Page
    , mUser : Maybe User
    , banners : Banners.Model
    }



-- Special type used to save/restore state via ports and flags;
-- Cannot use union types, and there's no need to keep the error message


type alias ModelForPorts =
    { pageStr : String
    , mUser : Maybe User
    }


convertModelForPort : Model -> ModelForPorts
convertModelForPort model =
    ModelForPorts (Page.toString model.page) model.mUser


convertModelFromPort : ModelForPorts -> Model
convertModelFromPort modelFP =
    Model "" "" Nothing (Page.fromString modelFP.pageStr) modelFP.mUser Banners.init


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

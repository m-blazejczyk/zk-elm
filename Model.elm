module Model exposing (
    Model, ModelForPorts, Msg(..), User, modelFromPort, userDecoder)

import Banners
import Issues
import Http
import Json.Decode as Decode exposing (..)
import Json.Encode as E
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
    | IssuesMsg Issues.Msg


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
    , issues : Issues.Model
    }



-- Special type used to save/restore state via ports and flags.
-- Cannot use union types, and 'Maybe' also doesn't work.


type alias ModelForPorts =
    { pageStr : String
    , user : User
    }


modelFromPort : ModelForPorts -> Model
modelFromPort modelFP =
    Model "" "" Nothing (Page.fromString modelFP.pageStr) (Just modelFP.user) Banners.init Issues.init


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

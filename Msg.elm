module Msg exposing (..)

import Http
import Page exposing (..)
import Model exposing (..)
import Banners


type Msg
    = ClickLogIn
    | SetUsername String
    | SetPassword String
    | GetTokenCompleted (Result Http.Error User)
    | LogOut
    | OpenPage Page
    | BannersMsg Banners.Msg

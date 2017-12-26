module Msg exposing (..)

import Http

import Page exposing (..)
import Model exposing (..)


type Msg
    = ClickLogIn
    | SetUsername String
    | SetPassword String
    | GetTokenCompleted (Result Http.Error User)
    | LogOut
    | OpenPage Page

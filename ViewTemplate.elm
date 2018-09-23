module ViewTemplate exposing (viewFooter, viewHeader, viewLoginForm, viewMainMenu, viewTitle, viewTopMenu, viewUserMenu)

import Global exposing (..)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Model exposing (..)
import Page exposing (..)


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
            [ a [ href "#", onClick (OpenPage MainMenu) ]
                [ img [ src (domain ++ "static/ZK_logo_red.png") ]
                    []
                ]
            ]
        , div [ id "user-menu" ]
            (viewUserMenu mUser)
        ]


viewTopMenu : Html Msg
viewTopMenu =
    let
        midIndex =
            (List.length editingPages // 2) - 1

        buildLi page =
            let
                ( short, _, long ) =
                    pageTitles page
            in
            li [] [ a [ href "#", title long, onClick (OpenPage page) ] [ text short ] ]

        mapper page t =
            let
                ( index, newList ) =
                    t
            in
            if index == midIndex then
                ( index + 1, li [ attribute "style" "width: 150px;" ] [] :: buildLi page :: newList )

            else
                ( index + 1, buildLi page :: newList )

        ( _, lis ) =
            List.foldr mapper ( 0, [] ) editingPages
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


viewLoginForm : String -> String -> Html Msg
viewLoginForm userName password =
    div [ class "jumbotron text-left" ]
        [ div [ id "form" ]
            [ div [ class "form-group row" ]
                [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "username" ] [ text "Kto?" ]
                    , input [ id "username", type_ "text", class "form-control", Html.Attributes.value userName, onInput SetUsername ] []
                    ]
                ]
            , div [ class "form-group row" ]
                [ div [ class "col-md-offset-2 col-md-8" ]
                    [ label [ for "password" ] [ text "Hasło:" ]
                    , input [ id "password", type_ "password", class "form-control", Html.Attributes.value password, onInput SetPassword ] []
                    ]
                ]
            , div [ class "text-center" ]
                [ button [ class "btn btn-primary", onClick ClickLogIn ] [ text "Zaloguj się" ]
                ]
            ]
        ]


viewTitle : Model -> Html Msg
viewTitle model =
    let
        page =
            if maybeIsJust model.mUser then
                model.page

            else
                MainMenu

        ( _, _, title ) =
            pageTitles page
    in
    h2 [ id "title" ] [ text title ]


viewMainMenu : Html Msg
viewMainMenu =
    let
        pageButton page active =
            let
                buttonClass isActive =
                    if isActive then
                        "btn-primary"

                    else
                        "btn-default"

                ( _, title, _ ) =
                    pageTitles page

                buttonText isActive =
                    if isActive then
                        text title

                    else
                        em [] [ text title ]
            in
            button
                [ class "btn"
                , class (buttonClass active)
                , onClick (OpenPage page)
                , style "width" "100%"
                , style "padding-top" "15px"
                , style "padding-bottom" "15px"
                , style "margin-top" "15px"
                , style "margin-bottom" "15px"
                ]
                [ buttonText active ]
    in
    div [ id "main-menu" ]
        [ div [ class "row" ]
            [ div [ class "col-md-4" ] [ pageButton Banners True ]
            , div [ class "col-md-4" ] [ pageButton HomePageContent False ]
            , div [ class "col-md-4" ] [ pageButton News False ]
            ]
        , div [ class "row" ]
            [ div [ class "col-md-4" ] [ pageButton Issues False ]
            , div [ class "col-md-4" ] [ pageButton Reviews False ]
            , div [ class "col-md-4" ] [ pageButton Repository False ]
            ]
        ]

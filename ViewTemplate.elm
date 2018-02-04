module ViewTemplate exposing (..)

import Html exposing (..)
import Html.Events exposing (..)
import Html.Attributes exposing (..)
import Global exposing (..)
import Page exposing (..)
import Model exposing (..)


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
            ((List.length editingPages) // 2) - 1

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
                    ( index + 1, (li [ attribute "style" "width: 150px;" ] []) :: (buildLi page) :: newList )
                else
                    ( index + 1, (buildLi page) :: newList )

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


viewTitle : Model -> Html Msg
viewTitle model =
    let
        page =
            if isLoggedIn model then
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
                buttonStyle =
                    [ ( "width", "100%" )
                    , ( "padding-top", "15px" )
                    , ( "padding-bottom", "15px" )
                    , ( "margin-top", "15px" )
                    , ( "margin-bottom", "15px" )
                    ]

                buttonClass active =
                    if active then
                        "btn-primary"
                    else
                        "btn-default"

                ( _, title, _ ) =
                    pageTitles page

                buttonText active =
                    if active then
                        text title
                    else
                        em [] [ text title ]
            in
                button
                    [ class "btn"
                    , class (buttonClass active)
                    , style buttonStyle
                    , onClick (OpenPage page)
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

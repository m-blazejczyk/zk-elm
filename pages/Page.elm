module Page exposing (Page(..), editingPages, pageTitles, toString, fromString)


type Page
    = MainMenu
    | News
    | Issues
    | Reviews
    | Repository
    | Banners
    | HomePageContent


pageTitles : Page -> ( String, String, String )
pageTitles page =
    case page of
        MainMenu ->
            ( "portal", "Portal", "Portal redakcyjny „Zeszytów Komiksowych”" )

        News ->
            ( "newsy", "Newsy", "Newsy" )

        Issues ->
            ( "numery", "Numery „ZK”", "Numery „Zeszytów Komiksowych”" )

        Reviews ->
            ( "recenzje", "Recenzje", "Recenzje" )

        Repository ->
            ( "składnica", "Składnica naukowa", "Składnica naukowa" )

        Banners ->
            ( "bannery", "Bannery", "Bannery" )

        HomePageContent ->
            ( "strona główna", "Strona główna", "Zawartość strony głównej" )


editingPages : List Page
editingPages =
    [ Banners, HomePageContent, News, Issues, Reviews, Repository ]


fromString : String -> Page
fromString str =
    case str of
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


toString : Page -> String
toString page =
    case page of
        News ->
            "News"

        Issues ->
            "Issues"

        Reviews ->
            "Reviews"

        Repository ->
            "Repository"

        Banners ->
            "Banners"

        HomePageContent ->
            "HomePageContent"

        MainMenu ->
            "MainMenu"

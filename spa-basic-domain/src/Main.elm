module Main exposing (main)

import Browser
import Browser.Navigation as Nav
import Html exposing (..)
import Html.Attributes exposing (..)
import Url
import Url.Parser as UP exposing ((</>))



-- MAIN


main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlChange = UrlChanged
        , onUrlRequest = UrlRequested
        }



-- URLS
{-
   /             => Home
   /contact      => Contact
   /help         => Help
   anything else => NotFound

-}


type Route
    = Home
    | Contact
    | Help
    | NotFound


routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.oneOf
        [ UP.map Home UP.top
        , UP.map Contact (UP.s "contact")
        , UP.map Help (UP.s "help")
        ]


urlToRoute : Url.Url -> Route
urlToRoute url =
    Maybe.withDefault NotFound (UP.parse routeParser url)



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , route : Route
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    ( { key = key
      , url = url
      , route = urlToRoute url
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = UrlChanged Url.Url
    | UrlRequested Browser.UrlRequest


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model
                | url = url
                , route = urlToRoute url
              }
            , Cmd.none
            )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        page =
            case model.route of
                Home ->
                    pageHome

                Contact ->
                    pageContact

                Help ->
                    pageHelp

                NotFound ->
                    pageNotFound model.url
    in
    { title = page.title ++ " | spa-basic-domain | elm-examples"
    , body =
        viewHeader
            ++ [ h1 [] [ text page.title ] ]
            ++ page.content
            ++ [ p [] [] ]
            ++ viewFooter model.route
    }


viewHeader : List (Html msg)
viewHeader =
    [ a [ href "https://www.markuslaire.com/github/elm-examples/index.html" ] [ text "Index" ]
    , hr [] []
    ]


viewExternalLink : String -> String -> Html msg
viewExternalLink linkText linkHref =
    a [ href linkHref, target "_blank" ] [ text linkText ]


viewLinkOrText : Route -> Route -> String -> String -> Html msg
viewLinkOrText currentRoute linkRoute linkText linkHref =
    if currentRoute == linkRoute then
        b [] [ text linkText ]

    else
        a [ href linkHref ] [ text linkText ]


viewFooter : Route -> List (Html msg)
viewFooter currentRoute =
    [ hr [] []
    , viewLinkOrText currentRoute Home "Home" "/"
    , text " ☆ "
    , viewLinkOrText currentRoute Contact "Contact" "/contact"
    , text " ☆ "
    , viewLinkOrText currentRoute Help "Help" "/help"
    ]



-- PAGE


type alias Page msg =
    { title : String
    , content : List (Html msg)
    }


pageHome : Page msg
pageHome =
    { title = "Home"
    , content =
        [ text "Welcome to this basic Elm SPA (Single Page Application) example."
        , br [] []
        , br [] []
        , text "Here's an invalid link to test the error page: "
        , a [ href "/abc/xyz" ] [ text "invalid link" ]
        ]
    }


pageContact : Page msg
pageContact =
    { title = "Contact"
    , content =
        [ text "This example was created by Markus Laire <malaire@gmail.com>."
        ]
    }


pageHelp : Page msg
pageHelp =
    { title = "Help"
    , content =
        [ text "Useful links:"
        , ul []
            [ li [] [ viewExternalLink "Official Guide - Web Apps" "https://guide.elm-lang.org/webapps/" ]
            ]
        , text "Apache setup I'm using:"
        , pre []
            [ text
                ("<Directory /PATH/TO/DOCUMENT/ROOT>\n"
                    ++ "Require all granted\n"
                    ++ "  FallbackResource /index.html\n"
                    ++ "</Directory>\n"
                )
            ]
        ]
    }


pageNotFound : Url.Url -> Page msg
pageNotFound url =
    { title = "Page not found"
    , content =
        [ text "ERROR: Page "
        , b [] [ text (Url.toString url) ]
        , text " not found."
        ]
    }

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
{- This app will be live at path /github/elm-examples/spa-basic

   All paths under that basepath should return (Internal NotFound),
   and all paths outside of that basepath should return External.
   But this doesn't seem to be possible currently with Url.Parser

   So as a workaround I'm handling specially the only External path
   this app is currently using, i.e. /github/elm-examples/index.html

   Current path mappings:

     /github/elm-examples/index.html        => (External "/github/elm-examples/index.html")
     /github/elm-examples/spa-basic         => (Internal Home)
     /github/elm-examples/spa-basic/contact => (Internal Contact)
     /github/elm-examples/spa-basic/help    => (Internal Help)
     anything else                          => (Internal NotFound)

-}


type Route
    = Internal InternalRoute
    | External String


type InternalRoute
    = Home
    | Contact
    | Help
    | NotFound


basepath : String
basepath =
    "/github/elm-examples/spa-basic"


routeParser : UP.Parser (Route -> a) a
routeParser =
    UP.s "github"
        </> UP.s "elm-examples"
        </> UP.oneOf
                [ UP.map (External "/github/elm-examples/index.html") (UP.s "index.html")
                , UP.s "spa-basic"
                    </> UP.oneOf
                            [ UP.map (Internal Home) UP.top
                            , UP.map (Internal Contact) (UP.s "contact")
                            , UP.map (Internal Help) (UP.s "help")
                            ]
                ]


urlToRoute : Url.Url -> Route
urlToRoute url =
    Maybe.withDefault (Internal NotFound) (UP.parse routeParser url)



-- MODEL


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , internalRoute : InternalRoute
    }


init : () -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init _ url key =
    let
        ( internalRoute, cmd ) =
            case urlToRoute url of
                Internal iroute ->
                    ( iroute, Cmd.none )

                External href ->
                    {- This case should be impossible, but handle it still properly -}
                    ( NotFound, Nav.load href )
    in
    ( { key = key
      , url = url
      , internalRoute = internalRoute
      }
    , cmd
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
            case urlToRoute url of
                Internal internalRoute ->
                    ( { model
                        | url = url
                        , internalRoute = internalRoute
                      }
                    , Cmd.none
                    )

                External href ->
                    ( model, Nav.load href )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Browser.Document Msg
view model =
    let
        page =
            case model.internalRoute of
                Home ->
                    pageHome

                Contact ->
                    pageContact

                Help ->
                    pageHelp

                NotFound ->
                    pageNotFound model.url
    in
    { title = page.title ++ " | spa-basic | elm-examples"
    , body =
        viewHeader
            ++ [ h1 [] [ text page.title ] ]
            ++ page.content
            ++ [ p [] [] ]
            ++ viewFooter model.internalRoute
    }


{-| link to elm-examples index, when live on my website
-}
viewHeader : List (Html msg)
viewHeader =
    [ a [ href "/github/elm-examples/index.html" ] [ text "Index" ]
    , hr [] []
    ]


viewExternalLink : String -> String -> Html msg
viewExternalLink linkText linkHref =
    a [ href linkHref, target "_blank" ] [ text linkText ]


viewLinkOrText : InternalRoute -> InternalRoute -> String -> String -> Html msg
viewLinkOrText currentRoute linkRoute linkText linkHref =
    if currentRoute == linkRoute then
        b [] [ text linkText ]

    else
        a [ href linkHref ] [ text linkText ]


viewFooter : InternalRoute -> List (Html msg)
viewFooter currentRoute =
    [ hr [] []
    , viewLinkOrText currentRoute Home "Home" basepath
    , text " ☆ "
    , viewLinkOrText currentRoute Contact "Contact" (basepath ++ "/contact")
    , text " ☆ "
    , viewLinkOrText currentRoute Help "Help" (basepath ++ "/help")
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
        , a [ href (basepath ++ "/abc/xyz") ] [ text "invalid link" ]
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
                ("<Directory /FILESYSTEM/PATH/TO/spa-basic>\n"
                    ++ "  FallbackResource "
                    ++ basepath
                    ++ "/index.html\n"
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

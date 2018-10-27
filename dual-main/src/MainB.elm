module MainB exposing (main)

import Browser
import Html exposing (..)



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view
        }



-- MODEL


type alias Model =
    ()


init : () -> ( Model, Cmd Msg )
init _ =
    ( ()
    , Cmd.none
    )



-- UPDATE


type Msg
    = Never


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Never ->
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    div [] [ text "Hello from MainB" ]

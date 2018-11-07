module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
import Task
import Time



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
    { time : Maybe Time.Posix }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { time = Nothing }, Cmd.none )



-- UPDATE


type Msg
    = GetTimeClick
    | GetTimeResult Time.Posix


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        GetTimeClick ->
            ( model, Task.perform GetTimeResult Time.now )

        GetTimeResult newTime ->
            ( { model | time = Just newTime }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> Html Msg
view model =
    let
        timeStr =
            case model.time of
                Just time ->
                    String.fromInt (Time.posixToMillis time)

                Nothing ->
                    "-"
    in
    div []
        [ button [ onClick GetTimeClick ] [ text "Get Time" ]
        , br [] []
        , br [] []
        , text ("Posix.Time: " ++ timeStr)
        ]

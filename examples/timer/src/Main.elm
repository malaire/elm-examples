module Main exposing (main)

import Browser
import Html exposing (..)
import Html.Events exposing (onClick)
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


type TimerState
    = Zeroed
    | Counting Time.Posix
    | Stopped Int


type alias Model =
    { currentTime : Time.Posix
    , timer : TimerState
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { currentTime = Time.millisToPosix 0
      , timer = Zeroed
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | StartTimer
    | StopTimer


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick newTime ->
            ( { model | currentTime = newTime }
            , Cmd.none
            )

        StartTimer ->
            ( { model | timer = Counting model.currentTime }
            , Cmd.none
            )

        StopTimer ->
            case model.timer of
                Counting startTime ->
                    ( { model | timer = Stopped (timeDiff model.currentTime startTime) }
                    , Cmd.none
                    )

                _ ->
                    ( model, Cmd.none )


timeDiff : Time.Posix -> Time.Posix -> Int
timeDiff timeA timeB =
    Time.posixToMillis timeA - Time.posixToMillis timeB



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 100 Tick



-- VIEW


view : Model -> Html Msg
view model =
    div []
        [ viewTimer model
        , button [ onClick StartTimer ] [ text "Start" ]
        , button [ onClick StopTimer ] [ text "Stop" ]
        ]


viewTimer : Model -> Html Msg
viewTimer model =
    let
        elapsed =
            case model.timer of
                Zeroed ->
                    0

                Counting startTime ->
                    timeDiff model.currentTime startTime

                Stopped elapsed_ ->
                    elapsed_
    in
    div []
        [ text (formatTimer elapsed)
        ]


formatTimer : Int -> String
formatTimer elapsed =
    let
        subseconds =
            modBy 10 (elapsed // 100)

        seconds =
            modBy 60 (elapsed // 1000)

        minutes =
            modBy 60 (elapsed // (60 * 1000))

        hours =
            modBy 60 (elapsed // (60 * 60 * 1000))
    in
    formatNumberTwoDigits hours
        ++ ":"
        ++ formatNumberTwoDigits minutes
        ++ ":"
        ++ formatNumberTwoDigits seconds
        ++ "."
        ++ String.fromInt subseconds


formatNumberTwoDigits : Int -> String
formatNumberTwoDigits number =
    if number < 10 then
        "0" ++ String.fromInt number

    else
        String.fromInt number

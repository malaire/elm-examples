module Main exposing (main)

import Browser
import Css exposing (..)
import Html
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (onClick)
import Http
import Json.Decode
import Task
import Time



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> toUnstyled
        }



-- MODEL


type alias TimeQuery =
    { offset : Int
    , roundTripDelay : Int
    }


type alias Model =
    { maybeClientTime : Maybe Time.Posix
    , maybeTimeQuery : Maybe TimeQuery
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { maybeClientTime = Nothing
      , maybeTimeQuery = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = Tick Time.Posix
    | QueryServerTime
    | GotServerTime (Result Http.Error TimeQuery)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        Tick clientTime ->
            let
                cmd =
                    case model.maybeTimeQuery of
                        Nothing ->
                            queryServerTime

                        Just _ ->
                            Cmd.none
            in
            ( { model | maybeClientTime = Just clientTime }, cmd )

        QueryServerTime ->
            ( model, queryServerTime )

        GotServerTime (Ok timeQuery) ->
            ( { model | maybeTimeQuery = Just timeQuery }, Cmd.none )

        GotServerTime (Err _) ->
            {- TODO: could show error message here -}
            ( model, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Time.every 50 Tick



-- VIEW


view : Model -> Html Msg
view model =
    let
        maybeServerTime =
            case ( model.maybeClientTime, model.maybeTimeQuery ) of
                ( Just clientTime, Just timeQuery ) ->
                    Just (timeAddMillis timeQuery.offset clientTime)

                ( _, _ ) ->
                    Nothing
    in
    div [ css [ textAlign center ] ]
        [ viewTime "Server Time (UTC)" maybeServerTime
        , br [] []
        , viewTime "Client Time (UTC)" model.maybeClientTime
        , viewTimeQuery model.maybeTimeQuery
        , br [] []
        , button [ onClick QueryServerTime ] [ text "Query server time" ]
        ]


viewTime : String -> Maybe Time.Posix -> Html Msg
viewTime title maybeTime =
    div []
        [ div [] [ text title ]
        , div [ css [ fontSize (pct 200), fontWeight bold ] ]
            [ text (formatTime maybeTime) ]
        ]


viewTimeQuery : Maybe TimeQuery -> Html Msg
viewTimeQuery maybeTimeQuery =
    case maybeTimeQuery of
        Just timeQuery ->
            div []
                [ div [ css [ fontSize (pct 125), fontWeight bold ] ]
                    [ text (formatOffset timeQuery.offset) ]
                , div [ css [ fontStyle italic ] ]
                    [ text ("delay " ++ String.fromInt timeQuery.roundTripDelay ++ " ms") ]
                ]

        Nothing ->
            div [] []


formatOffset : Int -> String
formatOffset offset =
    let
        offsetInTenths =
            offset // 100

        {- NOTE: sign is reversed here -}
        sign =
            if offsetInTenths > 0 then
                "- "

            else if offsetInTenths < 0 then
                "+ "

            else
                "± "

        tenths =
            String.fromInt (modBy 10 (abs offsetInTenths))

        seconds =
            String.fromInt (abs offsetInTenths // 10)
    in
    sign ++ seconds ++ "." ++ tenths ++ " s"


formatTime : Maybe Time.Posix -> String
formatTime maybeTime =
    case maybeTime of
        Nothing ->
            "--:--:--"

        Just time ->
            let
                seconds =
                    Time.toSecond Time.utc time

                minutes =
                    Time.toMinute Time.utc time

                hours =
                    Time.toHour Time.utc time
            in
            formatIntTwoDigits hours
                ++ ":"
                ++ formatIntTwoDigits minutes
                ++ ":"
                ++ formatIntTwoDigits seconds


formatIntTwoDigits : Int -> String
formatIntTwoDigits number =
    if number < 10 then
        "0" ++ String.fromInt number

    else
        String.fromInt number



-- HTTP


queryServerTime : Cmd Msg
queryServerTime =
    Time.now
        |> Task.andThen
            (\clientStartTime ->
                Http.get "https://www.markuslaire.com/ajax/TimeNowUTC.php" serverTimeDecoder
                    |> Http.toTask
                    |> Task.andThen
                        (\serverTime ->
                            Time.now
                                |> Task.andThen
                                    (\clientEndTime ->
                                        createTimeQuery clientStartTime serverTime clientEndTime
                                            |> Task.succeed
                                    )
                        )
            )
        |> Task.attempt GotServerTime


serverTimeDecoder : Json.Decode.Decoder Int
serverTimeDecoder =
    Json.Decode.field "time_ms" Json.Decode.int


createTimeQuery : Time.Posix -> Int -> Time.Posix -> TimeQuery
createTimeQuery clientStartTime serverTime clientEndTime =
    { offset = serverTime - timeAverageToMillis clientStartTime clientEndTime
    , roundTripDelay = timeDeltaToMillis clientEndTime clientStartTime
    }



-- TIME HELPERS


timeAddMillis : Int -> Time.Posix -> Time.Posix
timeAddMillis millis time =
    Time.millisToPosix (Time.posixToMillis time + millis)


{-| using integer division here gives wrong result, apparently because of Int overflow
<https://github.com/elm/compiler/issues/1832>
-}
timeAverageToMillis : Time.Posix -> Time.Posix -> Int
timeAverageToMillis timeA timeB =
    floor (toFloat (Time.posixToMillis timeA + Time.posixToMillis timeB) / 2)


timeDeltaToMillis : Time.Posix -> Time.Posix -> Int
timeDeltaToMillis timeA timeB =
    Time.posixToMillis timeA - Time.posixToMillis timeB

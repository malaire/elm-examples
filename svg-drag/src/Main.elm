module Main exposing (main)

import Array exposing (Array)
import Browser
import Css as C
import Html.Styled as H
import Html.Styled.Attributes as HA
import Json.Decode as Decode
import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Svg.Styled.Events as SE



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> H.toUnstyled
        }



-- MODEL


type alias Point =
    { x : Float
    , y : Float
    }


type alias Model =
    { points : Array Point
    , mousePos : Point
    , overPoint : Maybe Int
    , dragPoint : Maybe Int
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { points =
            Array.fromList
                [ { x = 25, y = 25 }
                , { x = 50, y = 50 }
                , { x = 75, y = 75 }
                ]
      , mousePos = Point 0 0
      , overPoint = Nothing
      , dragPoint = Nothing
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MouseMove Point
    | MouseOverPoint Int
    | MouseOutPoint Int
    | MouseDownPoint Int
    | MouseUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove mousePos ->
            case model.dragPoint of
                Just index ->
                    ( { model
                        | mousePos = mousePos
                        , points = Array.set index mousePos model.points
                      }
                    , Cmd.none
                    )

                _ ->
                    ( { model | mousePos = mousePos }, Cmd.none )

        MouseOverPoint index ->
            ( { model | overPoint = Just index }, Cmd.none )

        MouseOutPoint index ->
            if model.overPoint == Just index then
                ( { model | overPoint = Nothing }, Cmd.none )

            else
                ( model, Cmd.none )

        MouseDownPoint index ->
            ( { model | dragPoint = Just index }, Cmd.none )

        MouseUp ->
            ( { model | dragPoint = Nothing }, Cmd.none )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none



-- VIEW


view : Model -> H.Html Msg
view model =
    let
        xyStr =
            "("
                ++ String.fromInt (round model.mousePos.x)
                ++ ", "
                ++ String.fromInt (round model.mousePos.y)
                ++ ")"
    in
    H.div []
        [ S.svg
            [ SA.viewBox "0 0 100 100"
            , SA.css
                [ C.border3 (C.px 2) C.solid (C.hex "#888")

                -- make SVG text elements unselectable (EXPERIMENTAL CSS)
                , C.property "-moz-user-select" "none"
                , C.property "-webkit-user-select" "none"
                , C.property "-ms-user-select" "none"
                , C.property "user-select" "none"
                ]
            , SE.on "svgmousemove" (Decode.map MouseMove mouseMoveDecoder)
            , SE.onMouseUp MouseUp
            ]
            ([ S.rect
                [ SA.x "0.25"
                , SA.y "0.25"
                , SA.width "99.5"
                , SA.height "99.5"
                , SA.fill "none"
                , SA.stroke "#888"
                , SA.strokeWidth "0.5"
                ]
                []
             ]
                ++ (List.concat <|
                        List.indexedMap (viewPoint model) (Array.toList model.points)
                   )
                ++ [ S.text_
                        [ SA.x "1"
                        , SA.y "99"
                        , SA.fontSize "3"
                        ]
                        [ S.text xyStr ]
                   ]
            )
        ]


mouseMoveDecoder : Decode.Decoder Point
mouseMoveDecoder =
    Decode.map2 Point
        (Decode.at [ "detail", "x" ] Decode.float)
        (Decode.at [ "detail", "y" ] Decode.float)


viewPoint : Model -> Int -> Point -> List (S.Svg Msg)
viewPoint model index point =
    let
        radius =
            3.0

        fill =
            if model.dragPoint == Just index then
                "#C00"

            else if model.overPoint == Just index then
                "#0C0"

            else
                "#00C"
    in
    [ S.circle
        [ SA.cx (String.fromFloat point.x)
        , SA.cy (String.fromFloat point.y)
        , SA.r (String.fromFloat radius)
        , SA.fill fill
        , SE.onMouseOver (MouseOverPoint index)
        , SE.onMouseOut (MouseOutPoint index)
        , SE.onMouseDown (MouseDownPoint index)
        ]
        []
    , S.text_
        [ SA.x (String.fromFloat point.x)
        , SA.y (String.fromFloat (point.y - radius * 1.1))
        , SA.fontSize "3"
        , SA.fontWeight "bold"
        , SA.fill "#000"
        , SA.textAnchor "middle"
        ]
        [ S.text (String.fromInt index) ]
    ]

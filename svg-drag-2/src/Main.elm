port module Main exposing (main)

import Array exposing (Array)
import Browser
import Browser.Events
import Css as C
import Html.Styled as H
import Html.Styled.Attributes as HA
import Json.Decode as JD
import Json.Encode as JE
import Svg.Styled as S
import Svg.Styled.Attributes as SA
import Svg.Styled.Events as SE



-- CONSTANTS


svgElementId =
    "svg"



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = subscriptions
        , view = view >> H.toUnstyled
        }



-- PORTS


port requestScreenCtm : JE.Value -> Cmd msg


port receiveScreenCtm : (JE.Value -> msg) -> Sub msg



-- MODEL


type alias ScreenCtm =
    { a : Float
    , b : Float
    , c : Float
    , d : Float
    , e : Float
    , f : Float
    }


type alias Point =
    { x : Float
    , y : Float
    }


type alias Model =
    { points : Array Point
    , screenCtm : Maybe ScreenCtm
    , clientMousePos : Maybe Point
    , svgMousePos : Maybe Point
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
      , screenCtm = Nothing
      , clientMousePos = Nothing
      , svgMousePos = Nothing
      , overPoint = Nothing
      , dragPoint = Nothing
      }
    , requestScreenCtm (JE.string svgElementId)
    )



-- UPDATE


type Msg
    = MouseMove Point
    | MouseOverPoint Int
    | MouseOutPoint Int
    | MouseDownPoint Int
    | MouseUp
    | OnResize Int Int
    | GotScreenCtm JE.Value


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove clientMousePos ->
            let
                newSvgMousePos =
                    case model.screenCtm of
                        Just screenCtm ->
                            Just <| matrixTransform clientMousePos screenCtm

                        Nothing ->
                            Nothing

                newPoints =
                    case ( model.dragPoint, newSvgMousePos ) of
                        ( Just index, Just svgMousePos ) ->
                            Array.set index svgMousePos model.points

                        _ ->
                            model.points
            in
            ( { model
                | clientMousePos = Just clientMousePos
                , svgMousePos = newSvgMousePos
                , points = newPoints
              }
            , Cmd.none
            )

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

        OnResize _ _ ->
            ( model, requestScreenCtm (JE.string svgElementId) )

        GotScreenCtm value ->
            case JD.decodeValue screenCtmDecoder value of
                Ok screenCtm ->
                    ( { model | screenCtm = Just screenCtm }
                    , Cmd.none
                    )

                Err _ ->
                    -- probably failed because svg element wasn't created yet,
                    -- so request again
                    ( model, requestScreenCtm (JE.string svgElementId) )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ receiveScreenCtm GotScreenCtm
        , Browser.Events.onResize OnResize
        ]



-- VIEW


view : Model -> H.Html Msg
view model =
    let
        svgStr =
            case model.svgMousePos of
                Just { x, y } ->
                    "(" ++ String.fromInt (round x) ++ ", " ++ String.fromInt (round y) ++ ")"

                Nothing ->
                    "(-, -)"

        clientStr =
            case model.clientMousePos of
                Just { x, y } ->
                    "(" ++ String.fromInt (round x) ++ ", " ++ String.fromInt (round y) ++ ")"

                Nothing ->
                    "(-, -)"
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
            , SE.on "mousemove" (JD.map MouseMove mouseMoveDecoder)
            , SE.onMouseUp MouseUp
            , SA.id svgElementId
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
                        [ S.text <| "svg " ++ svgStr ++ " client " ++ clientStr ]
                   ]
            )
        ]


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



-- DECODERS


screenCtmDecoder : JD.Decoder ScreenCtm
screenCtmDecoder =
    JD.map6 (\a b c d e f -> { a = a, b = b, c = c, d = d, e = e, f = f })
        (JD.field "a" JD.float)
        (JD.field "b" JD.float)
        (JD.field "c" JD.float)
        (JD.field "d" JD.float)
        (JD.field "e" JD.float)
        (JD.field "f" JD.float)


mouseMoveDecoder : JD.Decoder Point
mouseMoveDecoder =
    JD.map2 (\x y -> { x = toFloat x, y = toFloat y })
        (JD.field "clientX" JD.int)
        (JD.field "clientY" JD.int)



-- MATH


{-| See <https://developer.mozilla.org/en-US/docs/Web/SVG/Attribute/transform>
-}
matrixTransform : Point -> ScreenCtm -> Point
matrixTransform p c =
    { x = c.a * p.x + c.c * p.y + c.e
    , y = c.b * p.x + c.d * p.y + c.f
    }

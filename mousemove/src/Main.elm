module Main exposing (main)

import Browser
import Css as C
import Html.Styled exposing (..)
import Html.Styled.Attributes exposing (css)
import Html.Styled.Events exposing (on)
import Json.Decode as JD



-- MAIN


main =
    Browser.element
        { init = init
        , update = update
        , subscriptions = always Sub.none
        , view = view >> toUnstyled
        }



-- MODEL


type alias MouseMoveData =
    { offsetX : Int
    , offsetY : Int
    , timeStamp : Float
    }


type alias Model =
    { mouseMove : MouseMoveData
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mouseMove =
            { offsetX = 0
            , offsetY = 0
            , timeStamp = 0.0
            }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MouseMove MouseMoveData


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove newData ->
            ( { model | mouseMove = newData }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ css [ C.height (C.vh 100) ]
        , on "mousemove" (JD.map MouseMove mouseMoveDecoder)
        ]
        [ text ("offsetX: " ++ String.fromInt model.mouseMove.offsetX)
        , br [] []
        , text ("offsetY: " ++ String.fromInt model.mouseMove.offsetY)
        , br [] []
        , text ("timeStamp: " ++ String.fromFloat model.mouseMove.timeStamp)
        ]


mouseMoveDecoder : JD.Decoder MouseMoveData
mouseMoveDecoder =
    JD.map3 MouseMoveData
        (JD.at [ "clientX" ] JD.int)
        (JD.at [ "clientY" ] JD.int)
        (JD.at [ "timeStamp" ] JD.float)

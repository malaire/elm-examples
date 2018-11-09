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


type alias MousePos =
    { offsetX : Int
    , offsetY : Int
    }


type alias Model =
    { mousePos : MousePos
    }


init : () -> ( Model, Cmd Msg )
init _ =
    ( { mousePos =
            { offsetX = 0
            , offsetY = 0
            }
      }
    , Cmd.none
    )



-- UPDATE


type Msg
    = MouseMove MousePos


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        MouseMove newPos ->
            ( { model | mousePos = newPos }, Cmd.none )



-- VIEW


view : Model -> Html Msg
view model =
    div
        [ css [ C.height (C.vh 100) ]
        , on "mousemove" (JD.map MouseMove mouseMoveDecoder)
        ]
        [ text ("offsetX: " ++ String.fromInt model.mousePos.offsetX)
        , br [] []
        , text ("offsetY: " ++ String.fromInt model.mousePos.offsetY)
        ]


mouseMoveDecoder : JD.Decoder MousePos
mouseMoveDecoder =
    JD.map2 MousePos
        (JD.at [ "clientX" ] JD.int)
        (JD.at [ "clientY" ] JD.int)

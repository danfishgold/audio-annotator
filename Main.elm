module Main exposing (..)

import Html exposing (Html, program)
import Html exposing (div, input, p, text)
import Html.Events exposing (onInput)
import Keyboard exposing (KeyCode)


type alias TimeStamp =
    Int


type alias Model =
    { url : String
    , timeStamp : TimeStamp
    , currentNote : String
    , notes : List ( TimeStamp, String )
    }


type Msg
    = EditUrl String
    | EditCurrentNote String
    | SetTimeStamp TimeStamp
    | KeyUp KeyCode


init : ( Model, Cmd Msg )
init =
    ( { url = "", timeStamp = 0, currentNote = "", notes = [] }, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Keyboard.ups KeyUp


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        _ ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    text "hey"


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

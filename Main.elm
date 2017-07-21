port module Main exposing (..)

import Html exposing (Html, program)
import Html exposing (div, input, p, h2, text)
import Html.Events exposing (onInput)
import Html.Attributes exposing (dir)
import Keyboard exposing (KeyCode)


type alias TimeStamp =
    Int


type alias Model =
    { url : String
    , ready : Bool
    , timeStamp : TimeStamp
    , currentNote : ( TimeStamp, String )
    , notes : List ( TimeStamp, String )
    }


type Msg
    = EditUrl String
    | IsReady Bool
    | EditCurrentNote String
    | AddNote ( TimeStamp, String )
    | SetTimeStamp TimeStamp
    | KeyUp KeyCode


port pauseUnpause : () -> Cmd msg


port seek : Int -> Cmd msg


port setUrl : String -> Cmd msg


port timeStamp : (TimeStamp -> msg) -> Sub msg


port isReady : (Bool -> msg) -> Sub msg


init : ( Model, Cmd Msg )
init =
    ( { url = ""
      , ready = False
      , timeStamp = 0
      , currentNote = ( 0, "" )
      , notes = []
      }
    , Cmd.none
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups KeyUp
        , timeStamp SetTimeStamp
        , isReady IsReady
        ]


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditUrl url ->
            ( { model | url = url }, setUrl url )

        IsReady ready ->
            ( { model | ready = ready }, Cmd.none )

        EditCurrentNote noteText ->
            let
                ( prevTimeStamp, prevText ) =
                    model.currentNote

                timeStamp =
                    if String.isEmpty prevText then
                        model.timeStamp
                    else
                        prevTimeStamp
            in
                ( { model | currentNote = ( timeStamp, noteText ) }, Cmd.none )

        SetTimeStamp timeStamp ->
            ( { model | timeStamp = timeStamp }, Cmd.none )

        AddNote newNote ->
            ( { model | notes = newNote :: model.notes }, Cmd.none )

        KeyUp 13 ->
            if not (String.isEmpty model.url) && not (String.isEmpty <| Tuple.second model.currentNote) then
                update (AddNote model.currentNote) model
            else
                ( model, Cmd.none )

        KeyUp 32 ->
            ( model, pauseUnpause () )

        KeyUp 37 ->
            ( model, seek 5 )

        KeyUp 39 ->
            ( model, seek -5 )

        KeyUp key ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ dir "rtl" ]
        [ p [] [ text "url" ]
        , input [ onInput EditUrl ] [ text model.url ]
        , p [] [ text "new note" ]
        , text <| toString <| Tuple.first model.currentNote
        , input [ onInput EditCurrentNote ] [ text <| Tuple.second model.currentNote ]
        , h2 [] [ text "notes" ]
        , model.notes |> List.map noteEntry |> div []
        ]


noteEntry : ( TimeStamp, String ) -> Html msg
noteEntry ( timeStamp, note ) =
    p [] [ text <| toString timeStamp ++ " " ++ note ]


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

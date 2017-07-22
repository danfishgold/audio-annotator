port module Main exposing (..)

import Html exposing (Html, program)
import Html exposing (div, input, p, b, span, h2, text)
import Html.Events exposing (onInput, onClick)
import Html.Attributes exposing (dir, value)
import Keyboard exposing (KeyCode)
import TimeStamp exposing (TimeStamp)
import Note exposing (Note)


type alias Model =
    { url : String
    , ready : Bool
    , timeStamp : TimeStamp
    , currentNote : Note
    , notes : List Note
    }


type Msg
    = EditUrl String
    | IsReady Bool
    | SetCurrentNoteText String
    | SetCurrentNoteTime TimeStamp
    | AddNote Note
    | DeleteNote Int
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
      , currentNote = Note 0 ""
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


listRemove : Int -> List a -> List a
listRemove idx lst =
    List.take idx lst ++ List.drop (idx + 1) lst


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditUrl url ->
            ( { model | url = url }, setUrl url )

        IsReady ready ->
            ( { model | ready = ready }, Cmd.none )

        SetCurrentNoteText noteText ->
            if model.currentNote.text == "" && noteText == " " then
                ( model, Cmd.none )
            else
                let
                    prevNote =
                        model.currentNote
                in
                    ( { model | currentNote = { prevNote | text = noteText } }
                    , Cmd.none
                    )

        SetCurrentNoteTime timeStamp ->
            let
                prevNote =
                    model.currentNote
            in
                ( { model | currentNote = { prevNote | timeStamp = timeStamp } }
                , Cmd.none
                )

        SetTimeStamp timeStamp ->
            let
                newModel =
                    { model | timeStamp = timeStamp }
            in
                if model.currentNote.text == "" then
                    update (SetCurrentNoteTime timeStamp) newModel
                else
                    ( newModel, Cmd.none )

        AddNote newNote ->
            ( { model | notes = newNote :: model.notes }, Cmd.none )

        DeleteNote index ->
            ( { model | notes = listRemove index model.notes }, Cmd.none )

        KeyUp 13 ->
            if not (String.isEmpty model.url) && not (String.isEmpty model.currentNote.text) then
                update (AddNote model.currentNote) { model | currentNote = Note model.timeStamp "" }
            else
                ( model, Cmd.none )

        KeyUp 32 ->
            if String.isEmpty model.currentNote.text then
                ( model, pauseUnpause () )
            else
                ( model, Cmd.none )

        KeyUp 37 ->
            if String.isEmpty model.currentNote.text then
                ( model, seek 5 )
            else
                ( model, Cmd.none )

        KeyUp 39 ->
            if String.isEmpty model.currentNote.text then
                ( model, seek -5 )
            else
                ( model, Cmd.none )

        KeyUp key ->
            ( model, Cmd.none )


view : Model -> Html Msg
view model =
    div [ dir "rtl" ]
        [ p [] [ text "url" ]
        , input [ onInput EditUrl, value model.url ] []
        , p [] [ text "new note" ]
        , text <| TimeStamp.asString model.currentNote.timeStamp
        , input [ onInput SetCurrentNoteText, value model.currentNote.text ] []
        , h2 [] [ text "notes" ]
        , model.notes |> List.map Note.view |> List.reverse |> div []
        ]


main : Program Never Model Msg
main =
    program
        { init = init
        , subscriptions = subscriptions
        , update = update
        , view = view
        }

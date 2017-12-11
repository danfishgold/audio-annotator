module Main exposing (..)

import Html exposing (Html, program)
import Html exposing (div, input, audio, span, h2, text)
import Html.Attributes exposing (attribute, hidden, dir, id, type_, accept, controls, style)
import Keyboard exposing (KeyCode)
import TimeStamp exposing (TimeStamp)
import Config exposing (Config)
import Source exposing (Source)
import Audio exposing (SeekDirection(..), SeekSize(..), controls)
import NoteTable exposing (view, SortOrder(..))
import Localization as Ln exposing (Locale)
import Note exposing (Note)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Button as Button


--MODEL


type alias Model =
    { ready : Bool
    , paused : Bool
    , timeStamp : TimeStamp
    , remainingTime : TimeStamp
    , currentNote : Note
    , notes : List Note
    , noteSortOrder : NoteTable.SortOrder
    , source : Source
    , config : Config
    }


type Msg
    = SetSource Source
    | IsReady Bool
    | SetNoteSortOrder NoteTable.SortOrder
    | PauseUnpause
    | Paused
    | Unpaused
    | Seek SeekSize SeekDirection
    | SetPlayhead TimeStamp
    | SetCurrentNoteText String
    | SetCurrentNoteTime TimeStamp
    | AddNote Note
    | DeleteNote Int
    | SetTimeStamp ( TimeStamp, TimeStamp )
    | KeyUp KeyCode
    | ConfigMsg Config.Msg



--INIT


init : String -> ( Model, Cmd Msg )
init url =
    ( { ready = False
      , paused = True
      , timeStamp = 0
      , remainingTime = 0
      , currentNote = Note 0 ""
      , notes = []
      , noteSortOrder = OldestFirst
      , source = Source.Url url
      , config = Config.default
      }
    , Audio.setUrl ( "audio", url )
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups KeyUp
        , Audio.timeStamp SetTimeStamp
        , Audio.isReady IsReady
        , Audio.paused (always Paused)
        , Audio.played (always Unpaused)
        ]



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSource source ->
            ( { model | source = source }
            , case source of
                Source.Url url ->
                    Audio.setUrl ( "audio", url )

                Source.File ->
                    Audio.setFileSource ( "file-input", "audio" )
            )

        IsReady ready ->
            ( { model | ready = ready }, Cmd.none )

        SetNoteSortOrder order ->
            ( { model | noteSortOrder = order }, Cmd.none )

        PauseUnpause ->
            ( model, Audio.pauseUnpause "audio" )

        Paused ->
            ( { model | paused = True }, Cmd.none )

        Unpaused ->
            ( { model | paused = False }, Cmd.none )

        Seek size direction ->
            let
                amount =
                    case size of
                        Small ->
                            model.config.smallSeek

                        Big ->
                            model.config.bigSeek

                value =
                    case direction of
                        Forward ->
                            amount

                        Backward ->
                            -amount
            in
                ( model, Audio.seek ( "audio", value ) )

        SetPlayhead timeStamp ->
            ( model, Audio.setPlayhead ( "audio", timeStamp ) )

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

        SetTimeStamp ( timeStamp, remainingTime ) ->
            let
                newModel =
                    { model
                        | timeStamp = timeStamp
                        , remainingTime = remainingTime
                    }
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
            -- Enter
            if model.ready && not (String.isEmpty model.currentNote.text) then
                update (AddNote model.currentNote)
                    { model | currentNote = Note model.timeStamp "" }
            else
                ( model, Cmd.none )

        KeyUp keyCode ->
            if model.ready && String.isEmpty model.currentNote.text then
                case keyCode of
                    32 ->
                        -- Space
                        update PauseUnpause model

                    39 ->
                        -- Right
                        update (Seek Small Forward) model

                    37 ->
                        -- Left
                        update (Seek Small Backward) model

                    38 ->
                        -- Up
                        update (Seek Big Forward) model

                    40 ->
                        -- Down
                        update (Seek Big Backward) model

                    key ->
                        ( model, Cmd.none )
            else
                ( model, Cmd.none )

        ConfigMsg message ->
            ( { model | config = Config.update message model.config }, Cmd.none )


listRemove : Int -> List a -> List a
listRemove idx lst =
    List.take idx lst ++ List.drop (idx + 1) lst



-- VIEW


view : Locale -> Model -> Html Msg
view locale model =
    Grid.container [ Ln.dir locale ]
        [ CDN.stylesheet
        , Html.map ConfigMsg (Config.localeSelect locale)
        , Source.view SetSource locale model.source
        , div
            [ hidden (not model.ready) ]
            [ Html.map ConfigMsg (Config.view locale model.config)
            , Audio.controls Seek PauseUnpause locale model.paused "50px"
            , h2 [] [ text (Ln.strings locale).allNotes ]
            , (Note.input SetCurrentNoteText locale)
                model.currentNote
                model.timeStamp
                model.remainingTime
            , if List.isEmpty model.notes then
                text ""
              else
                div []
                    [ Button.button
                        [ Button.roleLink
                        , Button.attrs
                            [ id "clipboard-copy-button"
                            , attribute "data-clipboard-text"
                                (Note.listToString model.notes)
                            ]
                        ]
                        [ text (Ln.strings locale).copyToClipboard ]
                    , (NoteTable.view SetNoteSortOrder SetPlayhead locale)
                        model.notes
                        model.noteSortOrder
                    ]
            ]
        ]



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init ""
        , subscriptions = subscriptions
        , update = update
        , view = \model -> view model.config.locale model
        }

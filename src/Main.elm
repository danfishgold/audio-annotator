module Main exposing (main)

import Array exposing (Array)
import Audio exposing (SeekDirection(..), SeekSize(..))
import Bootstrap.Button as Button
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Browser exposing (document)
import Browser.Events
import Config exposing (Config)
import Html exposing (Html, div, h2, text)
import Html.Attributes exposing (attribute, hidden, id)
import Json.Decode as Json
import Localization as Ln exposing (Locale)
import Note exposing (Note)
import NoteTable exposing (SortOrder(..))
import Source exposing (Source)
import TimeStamp exposing (TimeStamp)



--MODEL


type alias Model =
    { ready : Bool
    , paused : Bool
    , timeStamp : TimeStamp
    , remainingTime : TimeStamp
    , newNote : Note
    , notes : Array Note
    , currentNote : CurrentNote
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
    | SetNoteText CurrentNote String
    | SetNewNoteTime TimeStamp
    | AddNote Note
    | DeleteNote Int
    | SetTimeStamp ( TimeStamp, TimeStamp )
    | KeyUp Key
    | ConfigMsg Config.Msg


type CurrentNote
    = New
    | Existing Int



--INIT


init : String -> () -> ( Model, Cmd Msg )
init url () =
    ( { ready = False
      , paused = True
      , timeStamp = 0
      , remainingTime = 0
      , newNote = Note 0 ""
      , notes = Array.empty
      , currentNote = New
      , noteSortOrder = OldestFirst
      , source = Source.Url url
      , config = Config.default
      }
    , Audio.setUrl ( "audio", url )
    )



-- SUBSCRIPTIONS


subscriptions : Model -> Sub Msg
subscriptions _ =
    Sub.batch
        [ Browser.Events.onKeyUp (keyboardEventDecoder KeyUp)
        , Audio.timeStamp SetTimeStamp
        , Audio.isReady IsReady
        , Audio.paused (always Paused)
        , Audio.played (always Unpaused)
        ]


type Key
    = Enter
    | Space
    | Up
    | Down
    | Left
    | Right


keyboardEventDecoder : (Key -> msg) -> Json.Decoder msg
keyboardEventDecoder toMsg =
    Json.field "key" Json.string
        |> Json.andThen
            (\key ->
                case key of
                    "Enter" ->
                        Json.succeed (toMsg Enter)

                    " " ->
                        Json.succeed (toMsg Space)

                    "ArrowUp" ->
                        Json.succeed (toMsg Up)

                    "ArrowDown" ->
                        Json.succeed (toMsg Down)

                    "ArrowLeft" ->
                        Json.succeed (toMsg Left)

                    "ArrowRight" ->
                        Json.succeed (toMsg Right)

                    otherKey ->
                        Json.fail <| "Unrecognized key code: '" ++ otherKey ++ "'"
            )



-- UPDATE


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        SetSource source ->
            ( { model | source = source }, Source.reload source )

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

        SetNoteText _ noteText ->
            if model.newNote.text == "" && noteText == " " then
                ( model, Cmd.none )

            else
                let
                    note =
                        model.newNote
                in
                ( { model | newNote = { note | text = noteText } }
                , Cmd.none
                )

        SetNewNoteTime timeStamp ->
            let
                prevNote =
                    model.newNote
            in
            ( { model | newNote = { prevNote | timeStamp = timeStamp } }
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
            if model.newNote.text == "" then
                update (SetNewNoteTime timeStamp) newModel

            else
                ( newModel, Cmd.none )

        AddNote newNote ->
            ( { model | notes = Array.push newNote model.notes }, Cmd.none )

        DeleteNote index ->
            ( { model | notes = arrayRemove index model.notes }, Cmd.none )

        KeyUp Enter ->
            if model.ready && not (String.isEmpty model.newNote.text) then
                update (AddNote model.newNote)
                    { model | newNote = Note model.timeStamp "" }

            else
                ( model, Cmd.none )

        KeyUp someKey ->
            if model.ready && String.isEmpty model.newNote.text then
                case someKey of
                    Space ->
                        update PauseUnpause model

                    Right ->
                        update (Seek Small Forward) model

                    Left ->
                        update (Seek Small Backward) model

                    Up ->
                        update (Seek Big Forward) model

                    Down ->
                        update (Seek Big Backward) model

                    _ ->
                        ( model, Cmd.none )

            else
                ( model, Cmd.none )

        ConfigMsg message ->
            ( { model | config = Config.update message model.config }, Cmd.none )


arrayRemove : Int -> Array a -> Array a
arrayRemove idx arr =
    Array.append
        (Array.slice 0 idx arr)
        (Array.slice (idx + 1) (Array.length arr) arr)


currentNoteText : Model -> String
currentNoteText model =
    case model.currentNote of
        New ->
            model.newNote.text

        Existing index ->
            Array.get index model.notes
                |> Maybe.map .text
                |> Maybe.withDefault ""



-- VIEW


view : Locale -> Model -> Browser.Document Msg
view locale model =
    { title = (Ln.strings locale).title, body = [ body locale model ] }


body : Locale -> Model -> Html Msg
body locale model =
    Grid.container [ Ln.dir locale, Ln.textAlign locale ]
        [ CDN.stylesheet
        , Html.map ConfigMsg (Config.localeSelect locale)
        , Source.view SetSource locale model.source
        , div
            [ hidden (not model.ready) ]
            [ Html.map ConfigMsg (Config.view locale model.config)
            , Audio.controls Seek PauseUnpause model.paused "50px"
            , h2 [] [ text (Ln.strings locale).allNotes ]
            , Note.input
                (SetNoteText New)
                locale
                model.newNote
                model.timeStamp
                model.remainingTime
            , if Array.isEmpty model.notes then
                text ""

              else
                div []
                    [ Button.button
                        [ Button.roleLink
                        , Button.attrs
                            [ id "clipboard-copy-button"
                            , attribute "data-clipboard-text"
                                (Note.listToString <| Array.toList model.notes)
                            ]
                        ]
                        [ text (Ln.strings locale).copyToClipboard ]
                    , NoteTable.view
                        SetNoteSortOrder
                        SetPlayhead
                        locale
                        (Array.toList model.notes)
                        model.noteSortOrder
                    ]
            ]
        ]



-- MAIN


main : Program () Model Msg
main =
    document
        { init = init ""
        , subscriptions = subscriptions
        , update = update
        , view = \model -> view model.config.locale model
        }

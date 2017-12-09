module Main exposing (..)

import Html exposing (Html, program)
import Html exposing (div, input, audio, span, h2, text)
import Html.Attributes exposing (attribute, hidden, dir, id, type_, accept, controls, style)
import Html.Events exposing (onClick)
import Keyboard exposing (KeyCode)
import TimeStamp exposing (TimeStamp)
import Config exposing (Config)
import Source
import Audio exposing (SeekDirection(..), SeekSize(..), controls)
import Localization as Ln exposing (Locale)
import Note exposing (Note)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table exposing (th, tr, td)
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Button as Button
import Json.Decode as Json


--MODEL


type alias Model =
    { url : String
    , ready : Bool
    , paused : Bool
    , timeStamp : TimeStamp
    , remainingTime : TimeStamp
    , currentNote : Note
    , notes : List Note
    , noteSortOrder : NoteSortOrder
    , config : Config
    }


type Msg
    = EditUrl String
    | SetFileSource
    | IsReady Bool
    | SetNoteSortOrder NoteSortOrder
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


type NoteSortOrder
    = OldestFirst
    | NewestFirst



--INIT


init : String -> ( Model, Cmd Msg )
init url =
    ( { url = url
      , ready = False
      , paused = True
      , timeStamp = 0
      , remainingTime = 0
      , currentNote = Note 0 ""
      , notes = []
      , noteSortOrder = OldestFirst
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
        EditUrl url ->
            ( { model | url = url }, Audio.setUrl ( "audio", url ) )

        SetFileSource ->
            ( model, Audio.setFileSource ( "file-input", "audio" ) )

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
            let
                newModel =
                    case message of
                        Config.SetSourceType _ ->
                            { model | url = "", ready = False }

                        _ ->
                            model
            in
                ( { newModel | config = Config.update message model.config }
                , Cmd.none
                )


listRemove : Int -> List a -> List a
listRemove idx lst =
    List.take idx lst ++ List.drop (idx + 1) lst



-- VIEW


view : Locale -> Model -> Html Msg
view locale model =
    Grid.container [ Ln.dir locale ]
        [ CDN.stylesheet
        , Html.map ConfigMsg (Config.view locale model.config model.ready)
        , case model.config.sourceType of
            Source.UrlInput ->
                urlInput locale model

            Source.FileInput ->
                fileInput locale model
        , div [ hidden <| not model.ready ]
            [ Audio.controls Seek PauseUnpause locale model.paused "50px"
            , h2 [] [ text (Ln.strings locale).allNotes ]
            , noteInput locale model
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
                    , table locale model
                    ]
            ]
        ]


urlInput : Locale -> Model -> Html Msg
urlInput locale model =
    InputGroup.config
        (InputGroup.url
            [ Input.onInput EditUrl
            , Input.value model.url
            , Input.attrs [ dir "ltr" ]
            ]
        )
        |> Ln.predecessors locale
            [ InputGroup.span [] [ text (Ln.strings locale).fileUrl ]
            ]
        |> InputGroup.attrs [ dir "ltr" ]
        |> InputGroup.view


fileInput : Locale -> Model -> Html Msg
fileInput locale model =
    input
        [ type_ "file"
        , accept "audio/*"
        , id "file-input"
        , Html.Events.on "change" (Json.succeed SetFileSource)
        ]
        []


noteInput : Locale -> Model -> Html Msg
noteInput locale model =
    let
        currentTimeStamp =
            model.currentNote.timeStamp

        remainingTimeStamp =
            model.remainingTime - model.currentNote.timeStamp + model.timeStamp
    in
        Grid.row [ Row.middleXs ]
            [ Grid.col [ Col.xs2 ] [ text (Ln.strings locale).newNote ]
            , Grid.col [ Col.xs10 ]
                [ InputGroup.config
                    (InputGroup.text
                        [ Input.onInput SetCurrentNoteText
                        , Input.value model.currentNote.text
                        , Input.attrs [ Ln.dir locale ]
                        ]
                    )
                    |> Ln.predecessors locale
                        [ InputGroup.span []
                            [ text <| TimeStamp.asString currentTimeStamp ]
                        ]
                    |> Ln.successors locale
                        [ InputGroup.span []
                            [ text "-"
                            , text <| TimeStamp.asString remainingTimeStamp
                            ]
                        ]
                    |> InputGroup.attrs [ dir "ltr" ]
                    |> InputGroup.view
                ]
            ]


table : Locale -> Model -> Html Msg
table locale { notes, noteSortOrder } =
    let
        localized fn =
            Ln.strings locale |> fn

        sortOrderIndicator =
            case noteSortOrder of
                OldestFirst ->
                    "▲"

                NewestFirst ->
                    "▼"

        sortByTimeStamp =
            case noteSortOrder of
                OldestFirst ->
                    List.sortBy .timeStamp

                NewestFirst ->
                    List.sortBy (negate << .timeStamp)

        oppositeSortOrder =
            case noteSortOrder of
                OldestFirst ->
                    NewestFirst

                NewestFirst ->
                    OldestFirst

        head =
            Table.thead []
                [ tr []
                    [ th
                        [ Table.cellAttr <| style [ Ln.textAlign locale ]
                        , Table.cellAttr <| onClick (SetNoteSortOrder oppositeSortOrder)
                        ]
                        [ text <| localized .timeStamp ++ " " ++ sortOrderIndicator ]
                    , th
                        [ Table.cellAttr <|
                            style
                                [ Ln.textAlign locale
                                , ( "width", "100%" )
                                ]
                        ]
                        [ text <| localized .note ]
                    ]
                ]

        body =
            notes
                |> sortByTimeStamp
                |> List.map row
                |> Table.tbody []

        row note =
            let
                rowAttrs =
                    if String.startsWith "!" note.text then
                        [ Table.rowInfo ]
                    else
                        []
            in
                tr rowAttrs
                    [ td []
                        [ Button.button
                            [ Button.onClick (SetPlayhead note.timeStamp)
                            , Button.roleLink
                            ]
                            [ text <| TimeStamp.asString note.timeStamp ]
                        ]
                    , td
                        [ Table.cellAttr <|
                            style
                                [ ( "width", "100%" )
                                , ( "vertical-align", "middle" )
                                ]
                        ]
                        [ text note.text ]
                    ]
    in
        Table.table
            { options = [ Table.small ]
            , thead = head
            , tbody = body
            }



-- MAIN


main : Program Never Model Msg
main =
    program
        { init = init ""
        , subscriptions = subscriptions
        , update = update
        , view = \model -> view model.config.locale model
        }

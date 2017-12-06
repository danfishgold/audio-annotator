port module Main exposing (..)

import Html exposing (Html, program)
import Html exposing (div, input, p, b, span, h2, h5, text, audio, ul, li)
import Html.Attributes exposing (value, src, id, controls, style, class, dir, selected, hidden, type_, accept, attribute)
import Html.Events exposing (onClick, onInput)
import Keyboard exposing (KeyCode)
import TimeStamp exposing (TimeStamp)
import Config exposing (Config)
import Source
import Localization as L10N exposing (Locale)
import Note exposing (Note)
import Bootstrap.CDN as CDN
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Row as Row
import Bootstrap.Grid.Col as Col
import Bootstrap.Table as Table exposing (th, tr, td)
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Form.Select as Select
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Assets
import Json.Decode as Json


type alias Model =
    { url : String
    , ready : Bool
    , paused : Bool
    , timeStamp : TimeStamp
    , remainingTime : TimeStamp
    , currentNote : Note
    , notes : List Note
    , config : Config
    }


type Msg
    = EditUrl String
    | SetFileSource
    | IsReady Bool
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


type SeekDirection
    = Forward
    | Backward


type SeekSize
    = Small
    | Big


port pauseUnpause : String -> Cmd msg


port paused : (() -> msg) -> Sub msg


port played : (() -> msg) -> Sub msg


port seek : ( String, Int ) -> Cmd msg


port setPlayhead : ( String, Int ) -> Cmd msg


port setUrl : ( String, String ) -> Cmd msg


port setFileSource : ( String, String ) -> Cmd msg


port timeStamp : (( TimeStamp, TimeStamp ) -> msg) -> Sub msg


port isReady : (Bool -> msg) -> Sub msg


init : String -> ( Model, Cmd Msg )
init url =
    ( { url = url
      , ready = False
      , paused = True
      , timeStamp = 0
      , remainingTime = 0
      , currentNote = Note 0 ""
      , notes = []
      , config = Config.default
      }
    , setUrl ( "audio", url )
    )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.batch
        [ Keyboard.ups KeyUp
        , timeStamp SetTimeStamp
        , isReady IsReady
        , paused (always Paused)
        , played (always Unpaused)
        ]


listRemove : Int -> List a -> List a
listRemove idx lst =
    List.take idx lst ++ List.drop (idx + 1) lst


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        EditUrl url ->
            ( { model | url = url }, setUrl ( "audio", url ) )

        SetFileSource ->
            ( model, setFileSource ( "file-input", "audio" ) )

        IsReady ready ->
            ( { model | ready = ready }, Cmd.none )

        PauseUnpause ->
            ( model, pauseUnpause "audio" )

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
                ( model, seek ( "audio", value ) )

        SetPlayhead timeStamp ->
            ( model, setPlayhead ( "audio", timeStamp ) )

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
                    { model | timeStamp = timeStamp, remainingTime = remainingTime }
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
                update (AddNote model.currentNote) { model | currentNote = Note model.timeStamp "" }
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
                ( { newModel | config = Config.update message model.config }, Cmd.none )


view : Locale -> Model -> Html Msg
view locale model =
    Grid.container [ L10N.dir locale ]
        [ CDN.stylesheet
        , configView locale model
        , case model.config.sourceType of
            Source.UrlInput ->
                urlInput locale model

            Source.FileInput ->
                fileInput locale model
        , div [ hidden <| not model.ready ]
            [ audioControls locale model "50px"
            , h2 [] [ text (L10N.strings locale).allNotes ]
            , noteInput locale model
            , if List.isEmpty model.notes then
                text ""
              else
                div []
                    [ Button.button
                        [ Button.roleLink
                        , Button.attrs
                            [ id "clipboard-copy-button"
                            , attribute "data-clipboard-text" (allNoteText model.notes)
                            ]
                        ]
                        [ text (L10N.strings locale).copyToClipboard ]
                    , table locale model
                    ]
            ]
        ]


allNoteText : List Note -> String
allNoteText notes =
    notes
        |> List.map (\{ timeStamp, text } -> TimeStamp.asString timeStamp ++ "\t" ++ text)
        |> String.join "\n"


configView : Locale -> Model -> Html Msg
configView locale model =
    let
        localizedText fn =
            text (L10N.strings locale |> .config |> fn)

        stringToInt =
            String.toFloat >> Result.withDefault 0 >> floor

        sourceOption titleFn thisSource =
            ButtonGroup.radioButton (model.config.sourceType == thisSource)
                [ if model.config.sourceType == thisSource then
                    Button.primary
                  else
                    Button.secondary
                , Button.onClick <| ConfigMsg <| Config.SetSourceType thisSource
                ]
                [ localizedText titleFn ]

        sourceInput =
            ButtonGroup.radioButtonGroup
                [ ButtonGroup.small, ButtonGroup.attrs [ dir "ltr" ] ]
                [ sourceOption .localFile Source.FileInput
                , sourceOption .audioUrl Source.UrlInput
                ]

        seekInput selectedVal options message =
            Select.select
                [ Select.attrs
                    [ style
                        [ ( "display", "inline-block" )
                        , ( "width", "auto" )
                        ]
                    ]
                , Select.small
                , Select.onChange (stringToInt >> message)
                ]
                (options
                    |> List.map toString
                    |> List.map
                        (\val ->
                            Select.item [ value val, selected <| val == selectedVal ]
                                [ text val ]
                        )
                )

        smallSeekInput =
            seekInput (toString model.config.smallSeek)
                [ 2, 3, 5, 10 ]
                (ConfigMsg << Config.SetSmallSeek)

        bigSeekInput =
            seekInput (toString model.config.bigSeek)
                [ 15, 30, 60, 120 ]
                (ConfigMsg << Config.SetBigSeek)

        localeSelect =
            span [ locale |> L10N.next |> Config.SetLocale |> ConfigMsg |> onClick ] [ Assets.globe "3em" ]
    in
        div []
            [ localeSelect
            , h2 [] [ localizedText .title ]
            , ul []
                [ li [] [ localizedText .firstYouMustSupply, sourceInput ]
                , div [ hidden (not model.ready) ]
                    [ li []
                        [ localizedText .onLeftRightArrows
                        , smallSeekInput
                        , localizedText .seconds
                        ]
                    , li []
                        [ localizedText .onLeftRightButtons
                        , bigSeekInput
                        , localizedText .seconds
                        ]
                    , li [] [ localizedText .onSpace ]
                    , li [] [ localizedText .noteWithBang ]
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
        |> L10N.predecessors locale [ InputGroup.span [] [ text (L10N.strings locale).fileUrl ] ]
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


noUserSelect : List ( String, String )
noUserSelect =
    [ ( "-webkit-touch-callout", "none" )
    , ( "-webkit-user-select", "none" )
    , ( "-khtml-user-select", "none" )
    , ( "-moz-user-select", "none" )
    , ( "-ms-user-select", "none" )
    , ( "user-select", "none" )
    ]


audioControls : Locale -> Model -> String -> Html Msg
audioControls locale model sz =
    div []
        [ audio [ id "audio", controls False ] []
        , div [ dir "ltr", id "controls", style (( "text-align", "center" ) :: noUserSelect) ]
            [ span [ onClick (Seek Big Backward) ] [ Assets.previous sz ]
            , span [ onClick (Seek Small Backward) ] [ Assets.rewind sz ]
            , if model.paused then
                span [ onClick PauseUnpause ] [ Assets.play sz ]
              else
                span [ onClick PauseUnpause ] [ Assets.pause sz ]
            , span [ onClick (Seek Small Forward) ] [ Assets.fastForward sz ]
            , span [ onClick (Seek Big Forward) ] [ Assets.next sz ]
            ]
        ]


noteInput : Locale -> Model -> Html Msg
noteInput locale model =
    Grid.row [ Row.middleXs ]
        [ Grid.col [ Col.xs2 ] [ text (L10N.strings locale).newNote ]
        , Grid.col [ Col.xs10 ]
            [ InputGroup.config
                (InputGroup.text
                    [ Input.onInput SetCurrentNoteText
                    , Input.value model.currentNote.text
                    , Input.attrs [ L10N.dir locale ]
                    ]
                )
                |> L10N.predecessors locale
                    [ InputGroup.span []
                        [ text <| TimeStamp.asString model.currentNote.timeStamp ]
                    ]
                |> L10N.successors locale
                    [ InputGroup.span []
                        [ text "-"
                        , text <| TimeStamp.asString (model.remainingTime - model.currentNote.timeStamp + model.timeStamp)
                        ]
                    ]
                |> InputGroup.attrs [ dir "ltr" ]
                |> InputGroup.view
            ]
        ]


table : Locale -> Model -> Html Msg
table locale { notes } =
    let
        localizedText fn =
            text (L10N.strings locale |> fn)

        head =
            Table.thead []
                [ tr []
                    [ th [ Table.cellAttr <| style [ L10N.textAlign locale ] ]
                        [ localizedText .timeStamp ]
                    , th [ Table.cellAttr <| style [ L10N.textAlign locale, ( "width", "100%" ) ] ]
                        [ localizedText .note ]
                    ]
                ]

        body =
            notes
                |> List.sortBy .timeStamp
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
                        [ Button.button [ Button.onClick (SetPlayhead note.timeStamp), Button.roleLink ]
                            [ text <| TimeStamp.asString note.timeStamp ]
                        ]
                    , td [ Table.cellAttr <| style [ ( "width", "100%" ), ( "vertical-align", "middle" ) ] ] [ text note.text ]
                    ]
    in
        Table.table
            { options = [ Table.small ]
            , thead = head
            , tbody = body
            }


main : Program Never Model Msg
main =
    program
        { init = init ""
        , subscriptions = subscriptions
        , update = update
        , view = \model -> view model.config.locale model
        }

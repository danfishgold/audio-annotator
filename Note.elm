module Note exposing (Note, encode, decoder, input, listToString)

import Html exposing (Html, text)
import Html.Attributes exposing (dir)
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Bootstrap.Grid as Grid
import Bootstrap.Grid.Col as Col
import Bootstrap.Grid.Row as Row
import Json.Decode as Decode exposing (Decoder)
import Json.Encode as Encode
import Localization as Ln exposing (Locale)
import TimeStamp exposing (TimeStamp)


type alias Note =
    { timeStamp : TimeStamp
    , text : String
    }


decoder : Decoder Note
decoder =
    Decode.map2 Note
        (Decode.field "timeStamp" Decode.int)
        (Decode.field "text" Decode.string)


encode : Note -> Encode.Value
encode note =
    Encode.object
        [ ( "timeStamp", Encode.int note.timeStamp )
        , ( "text", Encode.string note.text )
        ]


listToString : List Note -> String
listToString notes =
    notes
        |> List.sortBy .timeStamp
        |> List.map toString
        |> String.join "\n"


toString : Note -> String
toString { timeStamp, text } =
    TimeStamp.asString timeStamp ++ "\t" ++ text


input : (String -> msg) -> Locale -> Note -> TimeStamp -> TimeStamp -> Html msg
input setCurrentNoteText locale currentNote timeStamp remainingTime =
    let
        currentTimeStamp =
            currentNote.timeStamp

        remainingTimeStamp =
            remainingTime - currentNote.timeStamp + timeStamp
    in
        Grid.row [ Row.middleXs ]
            [ Grid.col [ Col.xs2 ] [ text (Ln.strings locale).newNote ]
            , Grid.col [ Col.xs10 ]
                [ InputGroup.config
                    (InputGroup.text
                        [ Input.onInput setCurrentNoteText
                        , Input.value currentNote.text
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

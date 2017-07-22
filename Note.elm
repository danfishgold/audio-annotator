module Note exposing (Note, view)

import Html exposing (Html)
import Html exposing (span, p, b, text)
import TimeStamp exposing (TimeStamp)


type alias Note =
    { timeStamp : TimeStamp, text : String }


view : Note -> Html msg
view note =
    let
        time =
            span [] [ text <| TimeStamp.asString note.timeStamp ]

        content =
            if String.startsWith "!" note.text then
                b [] [ text note.text ]
            else
                span [] [ text note.text ]
    in
        p [] [ time, text " ", content ]

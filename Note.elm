module Note exposing (Note, encode, decoder)

import Html exposing (Html)
import Html exposing (span, p, b, text)
import TimeStamp exposing (TimeStamp)
import Json.Encode as Encode
import Json.Decode as Decode exposing (Decoder)


type alias Note =
    { timeStamp : TimeStamp, text : String }


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

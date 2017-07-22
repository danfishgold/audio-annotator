module Note exposing (Note)

import Html exposing (Html)
import Html exposing (span, p, b, text)
import TimeStamp exposing (TimeStamp)


type alias Note =
    { timeStamp : TimeStamp, text : String }

module TimeStamp exposing (TimeStamp, asString)


type alias TimeStamp =
    Int


asString : TimeStamp -> String
asString seconds =
    let
        h =
            seconds // 3600

        m =
            modBy 3600 seconds // 60

        s =
            modBy 60 seconds
    in
    if h == 0 then
        String.fromInt m ++ ":" ++ twoDigit s

    else
        String.fromInt h ++ ":" ++ twoDigit m ++ ":" ++ twoDigit s


twoDigit : TimeStamp -> String
twoDigit n =
    if n < 10 then
        "0" ++ String.fromInt n

    else
        String.fromInt n

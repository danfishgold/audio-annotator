module TimeStamp exposing (TimeStamp, asString)


type alias TimeStamp =
    Int


asString : TimeStamp -> String
asString seconds =
    let
        h =
            seconds // 3600

        m =
            (seconds % 3600) // 60

        s =
            seconds % 60
    in
        if h == 0 then
            toString m ++ ":" ++ twoDigit s
        else
            toString h ++ ":" ++ twoDigit m ++ ":" ++ twoDigit s


twoDigit : TimeStamp -> String
twoDigit n =
    if n < 10 then
        "0" ++ toString n
    else
        toString n

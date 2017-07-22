module Localization exposing (Locale(..), strings, dir, textAlign)

import Html
import Html.Attributes exposing (dir)


type Locale
    = Hebrew
    | English


dir : Locale -> Html.Attribute msg
dir locale =
    case locale of
        Hebrew ->
            Html.Attributes.dir "rtl"

        English ->
            Html.Attributes.dir "ltr"


textAlign : Locale -> ( String, String )
textAlign locale =
    case locale of
        Hebrew ->
            ( "text-align", "right" )

        English ->
            ( "text-align", "left" )


type alias Strings =
    { fileUrl : String
    , newNote : String
    , allNotes : String
    , timeStamp : String
    , note : String
    }


strings : Locale -> Strings
strings locale =
    case locale of
        Hebrew ->
            hebrewStrings

        English ->
            englishStrings


hebrewStrings : Strings
hebrewStrings =
    { fileUrl = "כתובת הקובץ"
    , newNote = "הערה חדשה"
    , allNotes = "הערות"
    , timeStamp = "זמן"
    , note = "הערה"
    }


englishStrings : Strings
englishStrings =
    { fileUrl = "File URL"
    , newNote = "New note"
    , allNotes = "Notes"
    , timeStamp = "Time"
    , note = "Note"
    }

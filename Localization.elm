module Localization exposing (Locale(..), strings, dir)

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


type alias Strings =
    { fileUrl : String
    , newNote : String
    , allNotes : String
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
    }


englishStrings : Strings
englishStrings =
    { fileUrl = "File URL"
    , newNote = "New note"
    , allNotes = "Notes"
    }

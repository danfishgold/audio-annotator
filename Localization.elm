module Localization exposing (Locale(..), next, strings, dir, textAlign, predecessors, successors)

import Html
import Html.Attributes exposing (dir)
import Bootstrap.Form.InputGroup as InputGroup


type Locale
    = Hebrew
    | English


next : Locale -> Locale
next locale =
    case locale of
        Hebrew ->
            English

        English ->
            Hebrew


predecessors : Locale -> List (InputGroup.Addon msg) -> InputGroup.Config msg -> InputGroup.Config msg
predecessors locale =
    case locale of
        Hebrew ->
            InputGroup.successors

        English ->
            InputGroup.predecessors


successors : Locale -> List (InputGroup.Addon msg) -> InputGroup.Config msg -> InputGroup.Config msg
successors locale =
    case locale of
        Hebrew ->
            InputGroup.predecessors

        English ->
            InputGroup.successors


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
    , config : ConfigStrings
    }


type alias ConfigStrings =
    { title : String
    , firstEnterUrl : String
    , onLeftRightArrows : String
    , onLeftRightButtons : String
    , seconds : String
    , onSpace : String
    , noteWithBang : String
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
    , config =
        { title = "הוראות / הגדרות"
        , firstEnterUrl = "קודם כל צריך לספק את הלינק לקובץ"
        , onLeftRightArrows = "לחיצה על החיצים ימינה ושמאלה במקלדת תזוז קדימה ואחורה ב"
        , onLeftRightButtons = "לחיצה על החיצים למעלה ולמטה במקלדת תזוז קדימה ואחורה ב"
        , seconds = "שניות"
        , onSpace = "לחיצה על רווח תעצור ותפעיל את הקובץ"
        , noteWithBang = "הערה שמתחילה בסימן קריאה תודגש"
        }
    }


englishStrings : Strings
englishStrings =
    { fileUrl = "File URL"
    , newNote = "New note"
    , allNotes = "Notes"
    , timeStamp = "Time"
    , note = "Note"
    , config =
        { title = "Instructions / Settings"
        , firstEnterUrl = "First enter the URL of the file you want to annotate"
        , onLeftRightArrows = "Use the left and right keyboard keys to move forward / back by "
        , onLeftRightButtons = "Use the up and down keyboard keys to move forward / back by "
        , seconds = "seconds"
        , onSpace = "Hitting the space key will play/pause the audio"
        , noteWithBang = "Notes that start with a bang (!) will be highlighted"
        }
    }

module Config exposing (Config, default, Msg(..), update)

import Localization as L10N exposing (Locale(..))
import Html exposing (Html, div, text)
import Source exposing (Type(..))


type alias Config =
    { smallSeek : Int
    , bigSeek : Int
    , locale : Locale
    , sourceType : Source.Type
    }


default : Config
default =
    { smallSeek = 5
    , bigSeek = 15
    , locale = Hebrew
    , sourceType = FileInput
    }


type Msg
    = SetSmallSeek Int
    | SetBigSeek Int
    | SetLocale Locale
    | SetSourceType Source.Type


update : Msg -> Config -> Config
update msg config =
    case msg of
        SetSmallSeek seek ->
            { config | smallSeek = seek }

        SetBigSeek seek ->
            { config | bigSeek = seek }

        SetLocale locale ->
            { config | locale = locale }

        SetSourceType sourceType ->
            { config | sourceType = sourceType }


view : Locale -> Config -> Html Msg
view locale config =
    text ""

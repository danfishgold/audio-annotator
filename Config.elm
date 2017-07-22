module Config exposing (Config, default, Msg(..), update)

import Localization as L10N exposing (Locale(..))
import Html exposing (Html, div, text)


type alias Config =
    { smallSeek : Int
    , bigSeek : Int
    , locale : Locale
    }


default : Config
default =
    { smallSeek = 5
    , bigSeek = 60
    , locale = Hebrew
    }


type Msg
    = SetSmallSeek Int
    | SetBigSeek Int
    | SetLocale Locale


update : Msg -> Config -> Config
update msg config =
    case msg of
        SetSmallSeek seek ->
            { config | smallSeek = seek }

        SetBigSeek seek ->
            { config | bigSeek = seek }

        SetLocale locale ->
            { config | locale = locale }


view : Locale -> Config -> Html Msg
view locale config =
    text ""

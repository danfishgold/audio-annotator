module Config exposing (Config, Msg(..), default, localeSelect, update, view)

import Assets
import Bootstrap.Form.Select as Select
import Html exposing (Html, div, h2, li, span, text, ul)
import Html.Attributes exposing (selected, style, value)
import Html.Events exposing (onClick)
import Localization as Ln exposing (Locale(..))


type alias Config =
    { smallSeek : Int
    , bigSeek : Int
    , locale : Locale
    }


default : Config
default =
    { smallSeek = 5
    , bigSeek = 15
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
    let
        localizedText fn =
            text (Ln.strings locale |> .config |> fn)

        stringToInt =
            String.toFloat >> Maybe.withDefault 0 >> floor

        seekInput selectedVal options message =
            Select.select
                [ Select.attrs
                    [ style "display" "inline-block"
                    , style "width" "auto"
                    ]
                , Select.small
                , Select.onChange (stringToInt >> message)
                ]
                (options
                    |> List.map Debug.toString
                    |> List.map
                        (\val ->
                            Select.item [ value val, selected <| val == selectedVal ]
                                [ text val ]
                        )
                )

        smallSeekInput =
            seekInput (Debug.toString config.smallSeek)
                [ 2, 3, 5, 10 ]
                SetSmallSeek

        bigSeekInput =
            seekInput (Debug.toString config.bigSeek)
                [ 15, 30, 60, 120 ]
                SetBigSeek
    in
    div []
        [ h2 [] [ localizedText .title ]
        , ul []
            [ li []
                [ localizedText .onLeftRightArrows
                , smallSeekInput
                , localizedText .seconds
                ]
            , li []
                [ localizedText .onLeftRightButtons
                , bigSeekInput
                , localizedText .seconds
                ]
            , li [] [ localizedText .onSpace ]
            , li [] [ localizedText .noteWithBang ]
            ]
        ]


localeSelect : Locale -> Html Msg
localeSelect locale =
    span [ locale |> Ln.next |> SetLocale |> onClick ] [ Assets.globe "3em" ]

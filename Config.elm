module Config exposing (Config, default, Msg(..), update, view)

import Localization as Ln exposing (Locale(..))
import Html exposing (Html, div, span, h2, ul, li, text)
import Html.Attributes exposing (dir, value, style, hidden, selected)
import Html.Events exposing (onClick)
import Source exposing (Type(..))
import Bootstrap.Form.Select as Select
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Assets


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


view : Locale -> Config -> Bool -> Html Msg
view locale config ready =
    let
        localizedText fn =
            text (Ln.strings locale |> .config |> fn)

        stringToInt =
            String.toFloat >> Result.withDefault 0 >> floor

        sourceOption titleFn thisSource =
            ButtonGroup.radioButton (config.sourceType == thisSource)
                [ if config.sourceType == thisSource then
                    Button.primary
                  else
                    Button.secondary
                , Button.onClick <| SetSourceType thisSource
                ]
                [ localizedText titleFn ]

        sourceInput =
            ButtonGroup.radioButtonGroup
                [ ButtonGroup.small, ButtonGroup.attrs [ dir "ltr" ] ]
                [ sourceOption .localFile Source.FileInput
                , sourceOption .audioUrl Source.UrlInput
                ]

        seekInput selectedVal options message =
            Select.select
                [ Select.attrs
                    [ style
                        [ ( "display", "inline-block" )
                        , ( "width", "auto" )
                        ]
                    ]
                , Select.small
                , Select.onChange (stringToInt >> message)
                ]
                (options
                    |> List.map toString
                    |> List.map
                        (\val ->
                            Select.item [ value val, selected <| val == selectedVal ]
                                [ text val ]
                        )
                )

        smallSeekInput =
            seekInput (toString config.smallSeek)
                [ 2, 3, 5, 10 ]
                SetSmallSeek

        bigSeekInput =
            seekInput (toString config.bigSeek)
                [ 15, 30, 60, 120 ]
                SetBigSeek

        localeSelect =
            span [ locale |> Ln.next |> SetLocale |> onClick ] [ Assets.globe "3em" ]
    in
        div []
            [ localeSelect
            , h2 [] [ localizedText .title ]
            , ul []
                [ li [] [ localizedText .firstYouMustSupply, sourceInput ]
                , div [ hidden (not ready) ]
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
            ]

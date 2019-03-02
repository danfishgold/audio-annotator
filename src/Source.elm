module Source exposing (Source(..), reload, view)

import Audio
import Bootstrap.Button as Button
import Bootstrap.ButtonGroup as ButtonGroup
import Bootstrap.Form.Input as Input
import Bootstrap.Form.InputGroup as InputGroup
import Html exposing (Html, div, h2, input, text)
import Html.Attributes exposing (accept, dir, hidden, id, type_)
import Html.Events
import Json.Decode as Json
import Localization as Ln exposing (Locale)


type Source
    = Url String
    | File


view : (Source -> msg) -> Locale -> Source -> Html msg
view setSource locale source =
    let
        localizedText fn =
            text (Ln.strings locale |> .source |> fn)
    in
    div []
        [ h2 [] [ localizedText .source ]
        , localizedText .firstYouMustSupply
        , selection setSource locale source
        , input setSource locale source
        ]


reload : Source -> Cmd msg
reload source =
    case source of
        Url url ->
            Audio.setUrl ( "audio", url )

        File ->
            Audio.setFileSource ( "file-input", "audio" )



-- SELECTION


selection : (Source -> msg) -> Locale -> Source -> Html msg
selection setSource locale source =
    let
        localizedText fn =
            text (Ln.strings locale |> .source |> fn)

        isFile =
            source == File
    in
    ButtonGroup.radioButtonGroup
        [ ButtonGroup.small, ButtonGroup.attrs [ dir "ltr" ] ]
        [ radioButton isFile (setSource File) (localizedText .localFile)
        , radioButton (not isFile) (setSource <| Url "") (localizedText .audioUrl)
        ]


radioButton : Bool -> msg -> Html msg -> ButtonGroup.RadioButtonItem msg
radioButton selected msg content =
    ButtonGroup.radioButton selected
        (if selected then
            [ Button.primary ]

         else
            [ Button.outlineSecondary, Button.onClick msg ]
        )
        [ content ]



-- INPUT


input : (Source -> msg) -> Locale -> Source -> Html msg
input setSource locale source =
    case source of
        Url url ->
            div []
                [ urlInput setSource locale url
                , div [ hidden True ] [ fileInput setSource ]
                ]

        File ->
            div []
                [ div [ hidden True ] [ urlInput setSource locale "" ]
                , fileInput setSource
                ]


urlInput : (Source -> msg) -> Locale -> String -> Html msg
urlInput setSource locale url =
    InputGroup.config
        (InputGroup.url
            [ Input.onInput (setSource << Url)
            , Input.value url
            , Input.attrs [ dir "ltr" ]
            ]
        )
        |> Ln.predecessors locale
            [ InputGroup.span [] [ text (Ln.strings locale).fileUrl ]
            ]
        |> InputGroup.attrs [ dir "ltr" ]
        |> InputGroup.view


fileInput : (Source -> msg) -> Html msg
fileInput setSource =
    Html.input
        [ type_ "file"
        , accept "audio/*"
        , id "file-input"
        , Html.Events.on "change" (Json.succeed <| setSource <| File)
        ]
        []

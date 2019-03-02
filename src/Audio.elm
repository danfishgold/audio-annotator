port module Audio exposing
    ( SeekDirection(..)
    , SeekSize(..)
    , controls
    , isReady
    , pauseUnpause
    , paused
    , played
    , seek
    , setFileSource
    , setPlayhead
    , setUrl
    , timeStamp
    )

import Assets
import Html exposing (Attribute, Html, audio, div, span)
import Html.Attributes exposing (dir, id, style)
import Html.Events exposing (onClick)
import TimeStamp exposing (TimeStamp)



-- SEEK


type SeekDirection
    = Forward
    | Backward


type SeekSize
    = Small
    | Big



-- PORTS


port pauseUnpause : String -> Cmd msg


port paused : (() -> msg) -> Sub msg


port played : (() -> msg) -> Sub msg


port seek : ( String, Int ) -> Cmd msg


port setPlayhead : ( String, Int ) -> Cmd msg


port setUrl : ( String, String ) -> Cmd msg


port setFileSource : ( String, String ) -> Cmd msg


port timeStamp : (( TimeStamp, TimeStamp ) -> msg) -> Sub msg


port isReady : (Bool -> msg) -> Sub msg



-- CONTROLS


type alias SeekMsg msg =
    SeekSize -> SeekDirection -> msg


controls : SeekMsg msg -> msg -> Bool -> String -> Html msg
controls onSeek onPauseUnpause isPaused sz =
    div []
        [ audio [ id "audio", Html.Attributes.controls False ] []
        , div
            ([ dir "ltr"
             , id "controls"
             , style "text-align" "center"
             ]
                ++ noUserSelect
            )
            [ span [ onClick (onSeek Big Backward) ] [ Assets.previous sz ]
            , span [ onClick (onSeek Small Backward) ] [ Assets.rewind sz ]
            , if isPaused then
                span [ onClick onPauseUnpause ] [ Assets.play sz ]

              else
                span [ onClick onPauseUnpause ] [ Assets.pause sz ]
            , span [ onClick (onSeek Small Forward) ] [ Assets.fastForward sz ]
            , span [ onClick (onSeek Big Forward) ] [ Assets.next sz ]
            ]
        ]


noUserSelect : List (Html.Attribute msg)
noUserSelect =
    [ style "-webkit-touch-callout" "none"
    , style "-webkit-user-select" "none"
    , style "-khtml-user-select" "none"
    , style "-moz-user-select" "none"
    , style "-ms-user-select" "none"
    , style "user-select" "none"
    ]

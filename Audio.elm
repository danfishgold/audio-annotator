port module Audio
    exposing
        ( SeekDirection(..)
        , SeekSize(..)
        , pauseUnpause
        , paused
        , played
        , seek
        , setPlayhead
        , setUrl
        , setFileSource
        , timeStamp
        , isReady
        , controls
        )

import TimeStamp exposing (TimeStamp)
import Localization as Ln exposing (Locale)
import Html exposing (Html, div, span, audio)
import Html.Attributes exposing (id, dir, style)
import Html.Events exposing (onClick)
import Assets


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


controls : (SeekSize -> SeekDirection -> msg) -> msg -> Locale -> Bool -> String -> Html msg
controls seek pauseUnpause locale paused sz =
    div []
        [ audio [ id "audio", Html.Attributes.controls False ] []
        , div [ dir "ltr", id "controls", style (( "text-align", "center" ) :: noUserSelect) ]
            [ span [ onClick (seek Big Backward) ] [ Assets.previous sz ]
            , span [ onClick (seek Small Backward) ] [ Assets.rewind sz ]
            , if paused then
                span [ onClick pauseUnpause ] [ Assets.play sz ]
              else
                span [ onClick pauseUnpause ] [ Assets.pause sz ]
            , span [ onClick (seek Small Forward) ] [ Assets.fastForward sz ]
            , span [ onClick (seek Big Forward) ] [ Assets.next sz ]
            ]
        ]


noUserSelect : List ( String, String )
noUserSelect =
    [ ( "-webkit-touch-callout", "none" )
    , ( "-webkit-user-select", "none" )
    , ( "-khtml-user-select", "none" )
    , ( "-moz-user-select", "none" )
    , ( "-ms-user-select", "none" )
    , ( "user-select", "none" )
    ]

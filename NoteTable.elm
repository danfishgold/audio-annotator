module NoteTable exposing (SortOrder(..), view)

import Html exposing (Html, text)
import Html.Attributes exposing (style)
import Html.Events exposing (onClick)
import Bootstrap.Button as Button
import Bootstrap.Table as Table exposing (th, tr, td)
import Localization as Ln exposing (Locale)
import Note exposing (Note)
import TimeStamp exposing (TimeStamp)


-- ORDER


type SortOrder
    = OldestFirst
    | NewestFirst


indicator : SortOrder -> String
indicator order =
    case order of
        OldestFirst ->
            "▲"

        NewestFirst ->
            "▼"


sort : SortOrder -> List Note -> List Note
sort order =
    case order of
        OldestFirst ->
            List.sortBy .timeStamp

        NewestFirst ->
            List.sortBy (negate << .timeStamp)


opposite : SortOrder -> SortOrder
opposite order =
    case order of
        OldestFirst ->
            NewestFirst

        NewestFirst ->
            OldestFirst



-- TABLE


head : (SortOrder -> msg) -> Locale -> SortOrder -> Table.THead msg
head setSortOrder locale order =
    let
        localized fn =
            Ln.strings locale |> fn
    in
        Table.thead []
            [ tr []
                [ th
                    [ Table.cellAttr <| style [ Ln.textAlign locale ]
                    , Table.cellAttr <| onClick <| setSortOrder <| opposite order
                    ]
                    [ text <| localized .timeStamp ++ " " ++ indicator order ]
                , th
                    [ Table.cellAttr <|
                        style
                            [ Ln.textAlign locale
                            , ( "width", "100%" )
                            ]
                    ]
                    [ text <| localized .note ]
                ]
            ]


row : (TimeStamp -> msg) -> Note -> Table.Row msg
row setPlayhead note =
    let
        rowAttrs =
            if String.startsWith "!" note.text then
                [ Table.rowInfo ]
            else
                []
    in
        tr rowAttrs
            [ td []
                [ Button.button
                    [ Button.onClick (setPlayhead note.timeStamp)
                    , Button.roleLink
                    ]
                    [ text <| TimeStamp.asString note.timeStamp ]
                ]
            , td
                [ Table.cellAttr <|
                    style
                        [ ( "width", "100%" )
                        , ( "vertical-align", "middle" )
                        ]
                ]
                [ text note.text ]
            ]


view : (SortOrder -> msg) -> (TimeStamp -> msg) -> Locale -> List Note -> SortOrder -> Html msg
view setSortOrder setPlayhead locale notes noteSortOrder =
    Table.table
        { options = [ Table.small ]
        , thead = head setSortOrder locale noteSortOrder
        , tbody =
            notes
                |> sort noteSortOrder
                |> List.map (row setPlayhead)
                |> Table.tbody []
        }

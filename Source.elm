module Source exposing (Type(..), Source(..))


type Type
    = UrlInput
    | FileInput


type Source
    = Url String
    | File

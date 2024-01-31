module Types exposing (..)


type BinaryDigits
    = One
    | Two
    | Three


type alias FrontendModel =
    { message : String
    , hashPrefixLen : Int
    , binaryDigits : BinaryDigits
    }


type alias BackendModel =
    { message : String
    }


type FrontendMsg
    = NoOpFrontendMsg
    | UpdateMessage String
    | UpdatePrefixLen String


type ToBackend
    = NoOpToBackend


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = NoOpToFrontend

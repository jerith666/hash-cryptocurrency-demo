module Types exposing (..)

import Browser exposing (UrlRequest)
import Browser.Navigation exposing (Key)
import Url exposing (Url)


type alias FrontendModel =
    { message : String
    , hashPrefixLen : Int
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

module Types exposing (..)

import Lamdera exposing (SessionId)


type BinaryDigits
    = One
    | Two
    | Three


type ClientRole
    = Student
    | Teacher


type AnonState
    = LoginUnattempted
    | LoginInProgress
    | LoginFailed


type FrontendModel
    = AnonFrontend String AnonState
    | LoggedIn FeModel


type alias FeModel =
    { message : String
    , hashPrefixLen : Int
    , binaryDigits : BinaryDigits
    , messages : List String
    , role : ClientRole
    }


type alias BackendModel =
    { teacher : Maybe SessionId
    , messages : List String
    }


type FrontendMsg
    = NoOpFrontendMsg
    | SetPassword String
    | Login
    | UpdateMessage String
    | UpdatePrefixLen String


type ToBackend
    = TeacherLogin String


type BackendMsg
    = NoOpBackendMsg


type ToFrontend
    = TeacherLoginOk
    | TeacherLoginBad

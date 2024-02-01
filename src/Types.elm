module Types exposing (..)

import Lamdera exposing (ClientId, SessionId)


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
    | WaitingForTeacher


type FrontendModel
    = AnonFrontend String AnonState
    | LoggedIn FeModel


type alias FeModel =
    { message : String
    , hashPrefixLen : Int
    , binaryDigits : BinaryDigits
    , messages : List String
    , shareRequests : List String
    , role : ClientRole
    }


type alias BackendModel =
    { teacher : Maybe SessionId
    , hashPrefixLen : Int
    , messages : List String
    }


type FrontendMsg
    = NoOpFrontendMsg
    | SetPassword String
    | Login
    | WaitForTeacher
    | UpdateMessage String
    | UpdatePrefixLenFe String
    | ShareMessageFe


type ToBackend
    = TeacherLogin String
    | UpdatePrefixLenBe Int
    | ShareMessage String


type BackendMsg
    = NewClient SessionId ClientId


type ToFrontend
    = TeacherLoginOk
    | TeacherLoginBad
    | TeacherArrived
    | PrefixLenUpdated Int
    | ShareMessageRequest String

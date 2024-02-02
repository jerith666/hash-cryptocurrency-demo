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
    , shareRequests : List String
    }


type FrontendMsg
    = NoOpFrontendMsg
    | SetPassword String
    | Login
    | WaitForTeacher
    | ReLogin
    | UpdateMessage String
    | UpdatePrefixLenFe String
    | ShareMessageFe
    | PermitMessageFe String
    | DenyMessageFe String
    | ClearMessagesFe


type ToBackend
    = TeacherLogin String
    | UpdatePrefixLenBe Int
    | ShareMessage String
    | PermitMessage String
    | DenyMessage String
    | ClearMessages


type BackendMsg
    = NewClient SessionId ClientId


type ToFrontend
    = TeacherLoginOk { hashPrefixLen : Int, messages : List String, shareRequests : List String }
    | TeacherLoginBad
    | TeacherArrived { hashPrefixLen : Int, messages : List String }
    | PrefixLenUpdated Int
    | ShareMessageRequest String
    | MessageShared String
    | MessagesCleared

module Evergreen.V1.Types exposing (..)

import Lamdera


type AnonState
    = LoginUnattempted
    | LoginInProgress
    | LoginFailed
    | WaitingForTeacher


type BinaryDigits
    = One
    | Two
    | Three


type ClientRole
    = Student
    | Teacher


type alias FeModel =
    { message : String
    , hashPrefixLen : Int
    , binaryDigits : BinaryDigits
    , messages : List String
    , shareRequests : List String
    , role : ClientRole
    }


type FrontendModel
    = AnonFrontend String AnonState
    | LoggedIn FeModel


type alias BackendModel =
    { teacher : Maybe Lamdera.SessionId
    , hashPrefixLen : Int
    , messages : List String
    , shareRequests : List String
    }


type FrontendMsg
    = NoOpFrontendMsg
    | SetPassword String
    | Login
    | WaitForTeacher
    | UpdateMessage String
    | UpdatePrefixLenFe String
    | ShareMessageFe
    | PermitMessageFe String
    | DenyMessageFe String


type ToBackend
    = TeacherLogin String
    | UpdatePrefixLenBe Int
    | ShareMessage String
    | PermitMessage String
    | DenyMessage String


type BackendMsg
    = NewClient Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = TeacherLoginOk
        { hashPrefixLen : Int
        , messages : List String
        , shareRequests : List String
        }
    | TeacherLoginBad
    | TeacherArrived
        { hashPrefixLen : Int
        , messages : List String
        }
    | PrefixLenUpdated Int
    | ShareMessageRequest String
    | MessageShared String

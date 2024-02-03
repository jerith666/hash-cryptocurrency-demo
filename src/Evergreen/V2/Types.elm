module Evergreen.V2.Types exposing (..)

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


type AutoHashing
    = Enabled Int
    | Disabled


type ClientRole
    = Student
    | Teacher


type alias FeModel =
    { message : String
    , autoHashSuffix : Maybe Int
    , hashPrefixLen : Int
    , binaryDigits : BinaryDigits
    , messages : List String
    , shareRequests : List String
    , autoHashing : AutoHashing
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
    , autoHashing : AutoHashing
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
    | DeleteMessageFe String
    | ClearMessagesFe
    | EnableAutoHashFe
    | DisableAutoHashFe
    | UpdateAutoHashPrefixLen String
    | AutoHash


type ToBackend
    = TeacherLogin String
    | UpdatePrefixLenBe Int
    | ShareMessage String
    | PermitMessage String
    | DenyMessage String
    | DeleteMessage String
    | ClearMessages
    | EnableAutoHash
    | DisableAutoHash


type BackendMsg
    = NewClient Lamdera.SessionId Lamdera.ClientId


type ToFrontend
    = TeacherLoginOk
        { hashPrefixLen : Int
        , messages : List String
        , shareRequests : List String
        , autoHashing : AutoHashing
        }
    | TeacherLoginBad
    | TeacherArrived
        { hashPrefixLen : Int
        , messages : List String
        , autoHashing : AutoHashing
        }
    | PrefixLenUpdated Int
    | ShareMessageRequest String
    | MessageShared String
    | MessageDeleted String
    | MessagesCleared
    | AutoHashEnabled Int
    | AutoHashDisabled

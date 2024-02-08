module Types exposing (..)

import Lamdera exposing (ClientId, SessionId)


type BinaryDigits
    = One
    | Two
    | Three


type ClientRole
    = Student
    | Teacher


type AutoHashing
    = Enabled Int
    | Disabled


type AnonState
    = LoginUnattempted
    | LoginInProgress
    | LoginFailed
    | WaitingForTeacher


type FrontendModel
    = AnonFrontend String AnonState
    | LoggedIn FeModel


type State
    = Active
    | Paused


type Name
    = Naming String
    | Named String


type alias FeModel =
    { name : Name
    , message : String
    , autoHashSuffix : Maybe Int
    , hashPrefixLen : Int
    , binaryDigits : BinaryDigits
    , messages : List String
    , shareRequests : List String
    , autoHashing : AutoHashing
    , role : ClientRole
    , state : State
    }


type alias BackendModel =
    { teacher : Maybe SessionId
    , hashPrefixLen : Int
    , messages : List String
    , shareRequests : List String
    , autoHashing : AutoHashing
    , state : State
    }


type FrontendMsg
    = NoOpFrontendMsg
    | SetPassword String
    | Login
    | WaitForTeacher
    | ReLogin
    | UpdateName String
    | AcceptName
    | ChangeStateFe State
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
    | ChangeState State
    | UpdatePrefixLenBe Int
    | ShareMessage String
    | PermitMessage String
    | DenyMessage String
    | DeleteMessage String
    | ClearMessages
    | EnableAutoHash
    | DisableAutoHash


type BackendMsg
    = NewClient SessionId ClientId


type ToFrontend
    = TeacherLoginOk
        { hashPrefixLen : Int
        , messages : List String
        , shareRequests : List String
        , autoHashing : AutoHashing
        , state : State
        }
    | TeacherLoginBad
    | TeacherArrived
        { hashPrefixLen : Int
        , messages : List String
        , autoHashing : AutoHashing
        , state : State
        }
    | StateChanged State
    | PrefixLenUpdated Int
    | ShareMessageRequest String
    | MessageShared String
    | MessageDeleted String
    | MessagesCleared
    | AutoHashEnabled Int
    | AutoHashDisabled

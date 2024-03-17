module Types exposing (..)

import Lamdera exposing (ClientId, SessionId)



-- front end


type FrontendModel
    = AnonFrontend String AnonState
    | LoggedIn LoggedInModel


type AnonState
    = LoginUnattempted
    | LoginInProgress
    | LoginFailed
    | WaitingForTeacher


type alias LoggedInModel =
    { name : Name
    , message : String
    , autoHashSuffix : Maybe Int
    , hashPrefixLen : Int
    , messages : List String
    , shareRequests : List String
    , autoHashing : AutoHashing
    , autoPush : AutoPush
    , role : ClientRole
    , state : State
    }


type Name
    = Naming String
    | Named String


type AutoHashing
    = Enabled Int
    | Disabled


type AutoPush
    = AutoPushEnabled
    | AutoPushDisabled


type ClientRole
    = Student
    | Teacher


type State
    = Active
    | Paused


type FrontendMsg
    = SetPassword String
    | Login
    | WaitForTeacher
    | ReLogin
    | UpdateName String
    | AcceptName
    | ChangeStateFe State
    | UpdateMessage String
    | UpdatePrefixLenFe String
    | ShareMessageFe
    | PushDraftMessageFe
    | ToggleAutoPush
    | PermitMessageFe String
    | DenyMessageFe String
    | DeleteMessageFe String
    | ClearMessagesFe
    | EnableAutoHashFe
    | DisableAutoHashFe
    | UpdateAutoHashPrefixLen String
    | AutoHash
    | NoOpFrontendMsg



-- back end


type alias BackendModel =
    { teacher : Maybe SessionId
    , hashPrefixLen : Int
    , draftMessage : Maybe String
    , messages : List String
    , shareRequests : List String
    , autoHashing : AutoHashing
    , state : State
    }


type BackendMsg
    = NewClient SessionId ClientId



-- front end <-> back end


type ToBackend
    = TeacherLogin String
    | ChangeState State
    | UpdatePrefixLenBe Int
    | ShareMessage String
    | PushDraftMessage String
    | PermitMessage String
    | DenyMessage String
    | DeleteMessage String
    | ClearMessages
    | EnableAutoHash
    | DisableAutoHash


type ToFrontend
    = TeacherLoginOk
        { hashPrefixLen : Int
        , draftMessage : Maybe String
        , messages : List String
        , shareRequests : List String
        , autoHashing : AutoHashing
        , state : State
        }
    | TeacherLoginBad
    | TeacherArrived
        { hashPrefixLen : Int
        , draftMessage : Maybe String
        , messages : List String
        , autoHashing : AutoHashing
        , state : State
        }
    | StateChanged State
    | PrefixLenUpdated Int
    | ShareMessageRequest String
    | MessageShared String
    | DraftMessagePushed String
    | MessageDeleted String
    | MessagesCleared
    | AutoHashEnabled Int
    | AutoHashDisabled

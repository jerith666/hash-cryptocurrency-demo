module Backend exposing (app)

import Env
import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, sendToFrontend)
import Types exposing (..)


type alias Model =
    BackendModel


app =
    Lamdera.backend
        { init = init
        , update = update
        , updateFromFrontend = updateFromFrontend
        , subscriptions = \_ -> onConnect NewClient
        }


init : ( Model, Cmd BackendMsg )
init =
    ( { teacher = Nothing
      , hashPrefixLen = 1
      , draftMessage = Nothing
      , messages = []
      , shareRequests = []
      , autoHashing = Disabled
      , state = Active
      }
    , Cmd.none
    )


update : BackendMsg -> Model -> ( Model, Cmd BackendMsg )
update msg model =
    case msg of
        NewClient sessionId clientId ->
            case model.teacher of
                Just t ->
                    case t == sessionId of
                        True ->
                            ( model
                            , sendToFrontend clientId <|
                                TeacherLoginOk
                                    { hashPrefixLen = model.hashPrefixLen
                                    , draftMessage = model.draftMessage
                                    , messages = model.messages
                                    , shareRequests = model.shareRequests
                                    , autoHashing = model.autoHashing
                                    , state = model.state
                                    }
                            )

                        False ->
                            ( model
                            , sendToFrontend clientId <|
                                TeacherArrived
                                    { hashPrefixLen = model.hashPrefixLen
                                    , draftMessage = model.draftMessage
                                    , messages = model.messages
                                    , autoHashing = model.autoHashing
                                    , state = model.state
                                    }
                            )

                Nothing ->
                    ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    let
        withTeacher modelCmd =
            case model.teacher of
                Nothing ->
                    unexpected msg model

                Just t ->
                    modelCmd t

        ifTeacher modelCmd =
            withTeacher
                (\t ->
                    case t == sessionId of
                        True ->
                            modelCmd

                        False ->
                            unexpected msg model
                )
    in
    case msg of
        TeacherLogin password ->
            case password == Env.teacherPassword of
                True ->
                    ( { model | teacher = Just sessionId }
                    , Cmd.batch
                        [ sendToFrontend clientId <|
                            TeacherLoginOk
                                { hashPrefixLen = model.hashPrefixLen
                                , draftMessage = model.draftMessage
                                , messages = model.messages
                                , shareRequests = model.shareRequests
                                , autoHashing = model.autoHashing
                                , state = model.state
                                }
                        , broadcast <|
                            TeacherArrived
                                { hashPrefixLen = model.hashPrefixLen
                                , draftMessage = model.draftMessage
                                , messages = model.messages
                                , autoHashing = model.autoHashing
                                , state = model.state
                                }
                        ]
                    )

                False ->
                    ( model, sendToFrontend clientId TeacherLoginBad )

        ChangeState state ->
            ifTeacher
                ( { model | state = state }
                , broadcast <| StateChanged state
                )

        UpdatePrefixLenBe newLen ->
            ifTeacher
                ( { model | hashPrefixLen = newLen }
                , broadcast <| PrefixLenUpdated newLen
                )

        ShareMessage message ->
            case List.member message model.shareRequests of
                True ->
                    ( model, Cmd.none )

                False ->
                    withTeacher
                        (\t ->
                            ( { model | shareRequests = model.shareRequests ++ [ message ] }
                            , sendToFrontend t <| ShareMessageRequest message
                            )
                        )

        PushDraftMessage message ->
            ifTeacher
                ( { model | draftMessage = Just message }
                , broadcast <| DraftMessagePushed message
                )

        PermitMessage message ->
            ifTeacher
                ( { model
                    | shareRequests = model.shareRequests |> List.filter ((/=) message)
                    , messages = model.messages ++ [ message ]
                  }
                , broadcast <| MessageShared message
                )

        DenyMessage message ->
            ifTeacher
                ( { model | shareRequests = model.shareRequests |> List.filter ((/=) message) }, Cmd.none )

        DeleteMessage message ->
            ifTeacher
                ( { model | messages = model.messages |> List.filter ((/=) message) }
                , broadcast <| MessageDeleted message
                )

        ClearMessages ->
            ifTeacher
                ( { model | messages = [] }, broadcast MessagesCleared )

        EnableAutoHash ->
            ifTeacher
                ( { model | autoHashing = Enabled 2 }
                , broadcast <| AutoHashEnabled 2
                )

        DisableAutoHash ->
            ifTeacher
                ( { model | autoHashing = Disabled }
                , broadcast <| AutoHashDisabled
                )


unexpected msg model =
    ( model, Cmd.none )

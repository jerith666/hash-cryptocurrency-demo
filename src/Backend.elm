module Backend exposing (app)

import Env
import Lamdera exposing (ClientId, SessionId, broadcast, onConnect, sendToFrontend)
import Task
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
      , messages = []
      , shareRequests = []
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
                                    , messages = model.messages
                                    , shareRequests = model.shareRequests
                                    }
                            )

                        False ->
                            ( model
                            , sendToFrontend clientId <|
                                TeacherArrived
                                    { hashPrefixLen = model.hashPrefixLen
                                    , messages = model.messages
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
                                , messages = model.messages
                                , shareRequests = model.shareRequests
                                }
                        , broadcast <|
                            TeacherArrived
                                { hashPrefixLen = model.hashPrefixLen
                                , messages = model.messages
                                }
                        ]
                    )

                False ->
                    ( model, sendToFrontend clientId TeacherLoginBad )

        UpdatePrefixLenBe newLen ->
            ifTeacher
                ( { model | hashPrefixLen = newLen }
                , broadcast <| PrefixLenUpdated newLen
                )

        ShareMessage message ->
            withTeacher
                (\t ->
                    ( { model | shareRequests = model.shareRequests ++ [ message ] }
                    , sendToFrontend t <| ShareMessageRequest message
                    )
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


unexpected msg model =
    ( model, Cmd.none )

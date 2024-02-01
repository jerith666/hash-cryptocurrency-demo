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
    ( { teacher = Nothing, messages = [] }
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
                            ( model, sendToFrontend clientId TeacherLoginOk )

                        False ->
                            ( model, sendToFrontend clientId TeacherArrived )

                Nothing ->
                    ( model, Cmd.none )


updateFromFrontend : SessionId -> ClientId -> ToBackend -> Model -> ( Model, Cmd BackendMsg )
updateFromFrontend sessionId clientId msg model =
    case msg of
        TeacherLogin password ->
            case password == Env.teacherPassword of
                True ->
                    ( { model | teacher = Just sessionId }
                    , Cmd.batch
                        [ sendToFrontend clientId TeacherLoginOk
                        , broadcast TeacherArrived
                        ]
                    )

                False ->
                    ( model, sendToFrontend clientId TeacherLoginBad )

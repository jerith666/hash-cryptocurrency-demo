module Frontend exposing (app)

import Binary
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Digits exposing (encodeBaseSixtyTwo)
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Lamdera exposing (sendToBackend)
import Process
import SHA
import String exposing (fromInt)
import Task exposing (Task)
import Types exposing (..)
import Url


type alias Model =
    FrontendModel


app =
    Lamdera.frontend
        { init = init
        , onUrlRequest = \_ -> NoOpFrontendMsg
        , onUrlChange = \_ -> NoOpFrontendMsg
        , update = update
        , updateFromBackend = updateFromBackend
        , subscriptions = \m -> Sub.none
        , view = view
        }


init : Url.Url -> Nav.Key -> ( Model, Cmd FrontendMsg )
init _ _ =
    ( AnonFrontend "" LoginUnattempted
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        SetPassword password ->
            case model of
                AnonFrontend _ state ->
                    ( AnonFrontend password state, Cmd.none )

                LoggedIn _ ->
                    unexpected msg model

        WaitForTeacher ->
            case model of
                AnonFrontend password _ ->
                    ( AnonFrontend password WaitingForTeacher, Cmd.none )

                LoggedIn _ ->
                    unexpected msg model

        Login ->
            case model of
                AnonFrontend password _ ->
                    ( AnonFrontend password LoginInProgress, sendToBackend <| TeacherLogin password )

                LoggedIn _ ->
                    unexpected msg model

        ReLogin ->
            ( AnonFrontend "" LoginUnattempted, Cmd.none )

        UpdateMessage newMsg ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | message = newMsg }, Cmd.none )

        UpdatePrefixLenFe newLenStr ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn _ ->
                    case String.toInt newLenStr of
                        Nothing ->
                            ( model, Cmd.none )

                        Just newLen ->
                            ( model, sendToBackend <| UpdatePrefixLenBe newLen )

        ShareMessageFe ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( model, sendToBackend <| ShareMessage m.message )

        PermitMessageFe message ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | shareRequests = m.shareRequests |> List.filter ((/=) message) }
                    , sendToBackend <| PermitMessage message
                    )

        DenyMessageFe message ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | shareRequests = m.shareRequests |> List.filter ((/=) message) }
                    , sendToBackend <| DenyMessage message
                    )

        DeleteMessageFe message ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | messages = m.messages |> List.filter ((/=) message) }
                    , sendToBackend <| DeleteMessage message
                    )

        ClearMessagesFe ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn _ ->
                    ( model, sendToBackend ClearMessages )

        EnableAutoHashFe ->
            ( model, sendToBackend EnableAutoHash )

        UpdateAutoHashPrefixLen lenStr ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    case String.toInt lenStr of
                        Nothing ->
                            unexpected msg model

                        Just len ->
                            ( LoggedIn { m | autoHashing = Enabled len }, Cmd.none )

        AutoHash ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    autohash m

        NoOpFrontendMsg ->
            ( model, Cmd.none )


autohash : FeModel -> ( Model, Cmd FrontendMsg )
autohash model =
    case model.autoHashing of
        Disabled ->
            ( LoggedIn model, Cmd.none )

        Enabled autoHashLen ->
            let
                hashed =
                    hash model.binaryDigits model.hashPrefixLen <| msgWithHashSuffix model

                prefixOk =
                    String.startsWith (String.repeat autoHashLen "0") hashed
            in
            case prefixOk of
                True ->
                    ( LoggedIn { model | message = msgWithHashSuffix model, autoHashSuffix = Nothing }
                    , Cmd.none
                    )

                False ->
                    ( LoggedIn { model | autoHashSuffix = updateSuffix model.autoHashSuffix }
                    , Process.sleep 1
                        |> Task.andThen (\_ -> Task.succeed AutoHash)
                        |> Task.perform identity
                    )


msgWithHashSuffix : FeModel -> String
msgWithHashSuffix m =
    case m.autoHashSuffix of
        Nothing ->
            m.message

        Just s ->
            m.message ++ encodeBaseSixtyTwo s


updateSuffix : Maybe Int -> Maybe Int
updateSuffix suffix =
    case suffix of
        Nothing ->
            Just 1

        Just s ->
            Just <| s + 1


unexpected _ model =
    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    let
        initModel beModel shareRequests role =
            { message = ""
            , autoHashSuffix = Nothing
            , hashPrefixLen = beModel.hashPrefixLen
            , binaryDigits = Three
            , messages = beModel.messages
            , shareRequests = shareRequests
            , autoHashing = Disabled
            , role = role
            }
    in
    case msg of
        TeacherLoginOk beModel ->
            case model of
                AnonFrontend _ _ ->
                    ( LoggedIn <| initModel beModel beModel.shareRequests Teacher, Cmd.none )

                LoggedIn _ ->
                    unexpected msg model

        TeacherLoginBad ->
            case model of
                AnonFrontend password _ ->
                    ( AnonFrontend password LoginFailed, Cmd.none )

                LoggedIn _ ->
                    unexpected msg model

        TeacherArrived beModel ->
            case model of
                AnonFrontend _ state ->
                    case state of
                        LoginInProgress ->
                            unexpected msg model

                        _ ->
                            ( LoggedIn <| initModel beModel [] Student, Cmd.none )

                LoggedIn m ->
                    case m.role of
                        Student ->
                            unexpected msg model

                        Teacher ->
                            -- someone else has taken over the Teacher role;
                            -- downgrade ourselves to a Student
                            ( LoggedIn <| initModel beModel [] Student, Cmd.none )

        PrefixLenUpdated newLen ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | hashPrefixLen = newLen }, Cmd.none )

        ShareMessageRequest message ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | shareRequests = m.shareRequests ++ [ message ] }, Cmd.none )

        MessageShared message ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | messages = m.messages ++ [ message ] }, Cmd.none )

        MessageDeleted message ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | messages = m.messages |> List.filter ((/=) message) }, Cmd.none )

        MessagesCleared ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | messages = [] }, Cmd.none )

        AutoHashEnabled autoHash ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | autoHashing = Enabled autoHash }, Cmd.none )


hash : BinaryDigits -> Int -> String -> String
hash binaryDigits prefixLen message =
    let
        binDigits =
            case binaryDigits of
                One ->
                    1

                Two ->
                    2

                Three ->
                    3
    in
    String.left prefixLen <|
        String.concat <|
            List.map String.fromInt <|
                List.map Binary.toDecimal <|
                    Binary.chunksOf binDigits <|
                        SHA.sha256 <|
                            Binary.fromStringAsUtf8 message


view : Model -> Browser.Document FrontendMsg
view model =
    { title = "Hash and Cryptocurrency Demo"
    , body =
        case model of
            AnonFrontend password state ->
                case state of
                    WaitingForTeacher ->
                        [ Html.text "waiting for teacher ..." ]

                    LoginInProgress ->
                        [ Html.text "logging in ..." ]

                    LoginFailed ->
                        [ Html.text "login failed" ]

                    LoginUnattempted ->
                        [ Html.div []
                            [ Html.input [ Attr.type_ "password", Attr.value password, Html.Events.onInput SetPassword ] []
                            , Html.button [ Html.Events.onClick Login ] [ Html.text "log in" ]
                            , Html.button [ Html.Events.onClick WaitForTeacher ] [ Html.text "wait for teacher" ]
                            ]
                        ]

            LoggedIn m ->
                viewFe m
    }


viewFe : FeModel -> List (Html FrontendMsg)
viewFe model =
    let
        hashFn =
            hash model.binaryDigits model.hashPrefixLen

        msgArea =
            Html.textarea
                [ Attr.value <| msgWithHashSuffix model
                , Attr.rows 8
                , Attr.cols 80
                , Html.Events.onInput UpdateMessage
                ]
                []

        prefixSpinner =
            Html.input
                [ Attr.type_ "number"
                , Attr.min "1"
                , Attr.max "128"
                , Attr.value <| fromInt model.hashPrefixLen
                , Html.Events.onInput UpdatePrefixLenFe
                ]
                []

        msgHash =
            Html.div
                [ Attr.style "font-family" "monospace" ]
                [ Html.text <| hashFn <| msgWithHashSuffix model ]

        shareButton =
            Html.button [ Html.Events.onClick ShareMessageFe ] [ Html.text "Share Message" ]

        autoHash =
            case model.autoHashing of
                Disabled ->
                    case model.role of
                        Teacher ->
                            Html.button [ Html.Events.onClick EnableAutoHashFe ]
                                [ Html.text "Enable Auto-Hashing" ]

                        Student ->
                            Html.span [ Attr.style "display" "none" ] []

                Enabled autoDigits ->
                    Html.div []
                        [ Html.input
                            [ Attr.type_ "number"
                            , Attr.min "1"
                            , Attr.max "10"
                            , Attr.value <| fromInt autoDigits
                            , Html.Events.onInput UpdateAutoHashPrefixLen
                            ]
                            []
                        , Html.button [ Html.Events.onClick AutoHash ] [ Html.text "Auto-Hash" ]
                        ]

        shareDecision message =
            [ Html.td [] [ Html.button [ Html.Events.onClick <| PermitMessageFe message ] [ Html.text "permit" ] ]
            , Html.td [] [ Html.button [ Html.Events.onClick <| DenyMessageFe message ] [ Html.text "deny" ] ]
            ]

        deleteButton message =
            [ Html.button [ Html.Events.onClick <| DeleteMessageFe message ] [ Html.text "delete" ] ]

        msgsTable messages shareMaker =
            Html.table [] <|
                [ Html.tr [] <| List.map (\t -> Html.td [] [ Html.text t ]) [ "message", "hash" ] ]
                    ++ List.map
                        (\m ->
                            Html.tr [ Attr.style "font-family" "monospace" ] <|
                                [ Html.td [ Attr.style "white-space" "pre" ] [ Html.text m ]
                                , Html.td [] [ Html.text <| hashFn m ]
                                ]
                                    ++ shareMaker m
                        )
                        messages

        clearMessages =
            Html.button [ Html.Events.onClick ClearMessagesFe ] [ Html.text "clear messages" ]

        teacherLogin =
            Html.button [ Html.Events.onClick ReLogin ] [ Html.text "T" ]
    in
    [ Html.div [] <|
        case model.role of
            Teacher ->
                [ msgArea
                , prefixSpinner
                , msgHash
                , autoHash
                , shareButton
                , msgsTable model.messages deleteButton
                , clearMessages
                , msgsTable model.shareRequests shareDecision
                ]

            Student ->
                [ msgArea
                , msgHash
                , autoHash
                , shareButton
                , teacherLogin
                , msgsTable model.messages <| always []
                ]
    ]

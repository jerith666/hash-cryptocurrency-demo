module Frontend exposing (app)

import BigInt
import Binary
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Digits exposing (encodeBaseSixtyTwo)
import Element as El
import Element.Background as Bg
import Element.Font as Font
import Element.Input as Inp
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Json.Decode
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
        , subscriptions = \_ -> Sub.none
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

        UpdateName name ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | name = Naming name }, Cmd.none )

        AcceptName ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    case m.name of
                        Naming n ->
                            ( LoggedIn
                                { m
                                    | name = Named n
                                    , message = String.replace "$name" n m.message
                                }
                            , Cmd.none
                            )

                        Named _ ->
                            unexpected msg model

        ChangeStateFe state ->
            ( model, sendToBackend <| ChangeState state )

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

        PushDraftMessageFe ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( model, sendToBackend <| PushDraftMessage m.message )

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
            ( model, sendToBackend ClearMessages )

        EnableAutoHashFe ->
            ( model, sendToBackend EnableAutoHash )

        DisableAutoHashFe ->
            ( model, sendToBackend DisableAutoHash )

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
                    hash model.hashPrefixLen <| msgWithHashSuffix model

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
            { name = Naming ""
            , message = Maybe.withDefault "" beModel.draftMessage
            , autoHashSuffix = Nothing
            , hashPrefixLen = beModel.hashPrefixLen
            , messages = beModel.messages
            , shareRequests = shareRequests
            , autoHashing = beModel.autoHashing
            , role = role
            , state = beModel.state
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

        StateChanged state ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | state = state }, Cmd.none )

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

        DraftMessagePushed message ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    let
                        subbedMessage =
                            case m.name of
                                Naming _ ->
                                    message

                                Named n ->
                                    String.replace "$name" n message
                    in
                    ( LoggedIn { m | message = subbedMessage }, Cmd.none )

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

        AutoHashDisabled ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    ( LoggedIn { m | autoHashing = Disabled }, Cmd.none )


hash : Int -> String -> String
hash prefixLen message =
    Binary.fromStringAsUtf8 message
        |> SHA.sha256
        |> Binary.toHex
        |> BigInt.fromHexString
        |> Maybe.map BigInt.toString
        |> Maybe.withDefault ""
        -- most significant digit will not be evenly distributed
        -- so just reverse it; nobody will notice
        |> String.reverse
        |> String.left prefixLen


splitLeadingZeros : String -> ( Maybe String, String )
splitLeadingZeros s =
    let
        foldOne c ( zeros, remainder ) =
            case remainder of
                "" ->
                    case c of
                        '0' ->
                            case zeros of
                                Nothing ->
                                    ( Just "0", "" )

                                Just zs ->
                                    ( Just <| String.cons '0' zs, "" )

                        _ ->
                            ( zeros, String.cons c remainder )

                _ ->
                    ( zeros, String.cons c remainder )
    in
    String.foldl foldOne ( Nothing, "" ) s


black : El.Color
black =
    El.rgb 0 0 0


white : El.Color
white =
    El.rgb 1 1 1


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
                        [ El.layout
                            [ El.width El.fill
                            , El.height El.fill
                            , Font.color white
                            , Bg.color black
                            ]
                          <|
                            El.column
                                [ El.width El.fill, El.height El.fill ]
                                [ Inp.button [ El.centerX, El.centerY ]
                                    { onPress = Just WaitForTeacher
                                    , label = El.text "Wait for the Teacher"
                                    }
                                , El.row [ El.padding 50, El.centerX, El.alignBottom ]
                                    [ Inp.currentPassword
                                        [ Bg.color black
                                        , Font.color white
                                        , Inp.focusedOnLoad
                                        , onEnter Login
                                        ]
                                        { onChange = SetPassword
                                        , text = password
                                        , placeholder = Just <| Inp.placeholder [] <| El.text "Teacher Password"
                                        , label = Inp.labelHidden "Teacher Password"
                                        , show = False
                                        }
                                    ]
                                ]
                        ]

            LoggedIn m ->
                viewFe m
    }


onEnter : msg -> El.Attribute msg
onEnter msg =
    El.htmlAttribute
        (Html.Events.on "keyup"
            (Json.Decode.field "key" Json.Decode.string
                |> Json.Decode.andThen
                    (\key ->
                        if key == "Enter" then
                            Json.Decode.succeed msg

                        else
                            Json.Decode.fail "Not the enter key"
                    )
            )
        )


viewFe : FeModel -> List (Html FrontendMsg)
viewFe model =
    let
        disabled =
            case model.role of
                Student ->
                    case model.state of
                        Active ->
                            Attr.disabled False

                        Paused ->
                            Attr.disabled True

                Teacher ->
                    Attr.disabled False

        hashFn =
            hash model.hashPrefixLen

        msgArea =
            Html.textarea
                (disabled
                    :: [ Attr.value <| msgWithHashSuffix model
                       , Attr.rows 8
                       , Attr.cols 80
                       , Html.Events.onInput UpdateMessage
                       ]
                )
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
                [ Html.span [] <|
                    case model.message of
                        "" ->
                            []

                        _ ->
                            let
                                ( zeros, remainder ) =
                                    splitLeadingZeros <| hashFn <| msgWithHashSuffix model
                            in
                            case zeros of
                                Nothing ->
                                    [ Html.text remainder ]

                                Just zs ->
                                    [ Html.span
                                        [ Attr.style "background" "grey"
                                        , Attr.style "color" "yellow"
                                        ]
                                        [ Html.text zs ]
                                    , Html.text remainder
                                    ]
                ]

        shareButton =
            Html.button (disabled :: [ Html.Events.onClick ShareMessageFe ]) [ Html.text "→ Share" ]

        pushDraftButton =
            Html.button [ Html.Events.onClick PushDraftMessageFe ] [ Html.text "Push" ]

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
                    Html.div [] <|
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
                            ++ (case model.role of
                                    Student ->
                                        []

                                    Teacher ->
                                        [ Html.button [ Html.Events.onClick DisableAutoHashFe ]
                                            [ Html.text "Disable Auto-Hash" ]
                                        ]
                               )

        shareDecision message =
            [ Html.td [] [ Html.button [ Html.Events.onClick <| PermitMessageFe message ] [ Html.text "permit" ] ]
            , Html.td [] [ Html.button [ Html.Events.onClick <| DenyMessageFe message ] [ Html.text "deny" ] ]
            ]

        deleteButton message =
            [ Html.button [ Html.Events.onClick <| DeleteMessageFe message ] [ Html.text "delete" ] ]

        msgsTable messages shareMaker =
            case messages of
                [] ->
                    Html.span [ Attr.style "display" "none" ] []

                _ ->
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

        otherState =
            case model.state of
                Active ->
                    Paused

                Paused ->
                    Active

        changeStateLabel =
            case model.state of
                Active ->
                    "Pause"

                Paused ->
                    "Activate"

        changeState =
            Html.button [ Html.Events.onClick <| ChangeStateFe otherState ]
                [ Html.text changeStateLabel ]
    in
    case model.name of
        Naming n ->
            [ Html.div []
                [ Html.text "Enter your name: "
                , Html.input [ Attr.value n, Html.Events.onInput UpdateName ] []
                , Html.button [ Html.Events.onClick AcceptName ] [ Html.text "OK" ]
                ]
            ]

        Named n ->
            [ Html.div [] <|
                case model.role of
                    Teacher ->
                        [ Html.text n
                        , msgArea
                        , prefixSpinner
                        , msgHash
                        , autoHash
                        , changeState
                        , pushDraftButton
                        , shareButton
                        , msgsTable model.messages deleteButton
                        , clearMessages
                        , msgsTable model.shareRequests shareDecision
                        ]

                    Student ->
                        [ Html.text n
                        , msgArea
                        , shareButton
                        , msgHash
                        , autoHash
                        , msgsTable model.messages <| always []
                        , teacherLogin
                        ]
            ]

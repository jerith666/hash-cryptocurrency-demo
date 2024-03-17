module Frontend exposing (app)

import BigInt
import Binary
import Browser exposing (UrlRequest(..))
import Browser.Dom
import Browser.Navigation as Nav
import Digits exposing (encodeBaseSixtyTwo)
import Element as El
import Element.Background as Bg
import Element.Border as Border
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


init : Url.Url -> Nav.Key -> ( FrontendModel, Cmd FrontendMsg )
init _ _ =
    ( AnonFrontend "" LoginUnattempted
    , Cmd.none
    )


update : FrontendMsg -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
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
                    ( AnonFrontend password LoginInProgress
                    , sendToBackend <| TeacherLogin password
                    )

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
                            , focusById msgTextAreaId
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
                    ( LoggedIn { m | message = newMsg, autoHashSuffix = Nothing }
                    , case m.autoPush of
                        AutoPushDisabled ->
                            Cmd.none

                        AutoPushEnabled ->
                            sendToBackend <| PushDraftMessage newMsg
                    )

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

        ToggleAutoPush ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    case m.autoPush of
                        AutoPushDisabled ->
                            ( LoggedIn { m | autoPush = AutoPushEnabled }, Cmd.none )

                        AutoPushEnabled ->
                            ( LoggedIn { m | autoPush = AutoPushDisabled }, Cmd.none )

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


autohash : LoggedInModel -> ( FrontendModel, Cmd FrontendMsg )
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


msgWithHashSuffix : LoggedInModel -> String
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


focusById : String -> Cmd FrontendMsg
focusById id =
    Task.attempt (\_ -> NoOpFrontendMsg) <| Browser.Dom.focus id


updateFromBackend : ToFrontend -> FrontendModel -> ( FrontendModel, Cmd FrontendMsg )
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
            , autoPush = AutoPushDisabled
            , role = role
            , state = beModel.state
            }
    in
    case msg of
        TeacherLoginOk beModel ->
            case model of
                AnonFrontend _ _ ->
                    ( LoggedIn <| initModel beModel beModel.shareRequests Teacher
                    , focusById nameTextFieldId
                    )

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
                            ( LoggedIn <| initModel beModel [] Student
                            , focusById nameTextFieldId
                            )

                LoggedIn m ->
                    case m.role of
                        Student ->
                            unexpected msg model

                        Teacher ->
                            -- someone else has taken over the Teacher role;
                            -- downgrade ourselves to a Student
                            ( LoggedIn <| initModel beModel [] Student
                            , focusById nameTextFieldId
                            )

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
                    case m.role of
                        Teacher ->
                            -- network delays mean draft message pushes can clobber
                            -- quick typing on the teacher side; ignore them since
                            -- the teacher by definition already has the pushed message
                            ( model, Cmd.none )

                        Student ->
                            let
                                subbedMessage =
                                    case m.name of
                                        Naming _ ->
                                            message

                                        Named n ->
                                            case m.role of
                                                Student ->
                                                    String.replace "$name" n message

                                                Teacher ->
                                                    message
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
                                    ( Just <| zs ++ "0", "" )

                        _ ->
                            ( zeros, remainder ++ String.fromChar c )

                _ ->
                    ( zeros, remainder ++ String.fromChar c )
    in
    String.foldl foldOne ( Nothing, "" ) s


black : El.Color
black =
    El.rgb 0 0 0


white : El.Color
white =
    El.rgb 1 1 1


darkgray : El.Color
darkgray =
    El.rgb 0.3 0.3 0.3


darkishgray : El.Color
darkishgray =
    El.rgb 0.375 0.375 0.375


yellow : El.Color
yellow =
    El.rgb 1 1 0


fullScreenWhiteOnBlack : El.Element msg -> Html msg
fullScreenWhiteOnBlack =
    El.layout
        [ El.width El.fill
        , El.height El.fill
        , Font.color white
        , Bg.color black
        ]


styledButton : List (El.Attribute msg) -> { label : El.Element msg, onPress : Maybe msg } -> El.Element msg
styledButton attrs =
    Inp.button <|
        [ Border.color white
        , Border.solid
        , Border.width 2
        , Border.rounded 7
        , El.padding 5
        ]
            ++ attrs


view : FrontendModel -> Browser.Document FrontendMsg
view model =
    { title = "Hash and Cryptocurrency Demo"
    , body =
        case model of
            AnonFrontend password state ->
                viewAnon password state

            LoggedIn m ->
                viewLoggedIn m
    }


viewAnon : String.String -> AnonState -> List (Html FrontendMsg)
viewAnon password state =
    case state of
        WaitingForTeacher ->
            [ fullScreenWhiteOnBlack <|
                El.el [ El.centerX, El.centerY ] <|
                    El.text "Waiting for Teacher ..."
            ]

        LoginInProgress ->
            [ fullScreenWhiteOnBlack <|
                El.el [ El.centerX, El.centerY ] <|
                    El.text "logging in ..."
            ]

        LoginFailed ->
            [ fullScreenWhiteOnBlack <|
                El.el [ El.centerX, El.centerY ] <|
                    El.text "login failed"
            ]

        LoginUnattempted ->
            [ fullScreenWhiteOnBlack <|
                El.column
                    [ El.width El.fill, El.height El.fill ]
                    [ styledButton [ El.centerX, El.centerY ]
                        { onPress = Just WaitForTeacher
                        , label = El.text "Wait for the Teacher"
                        }
                    , El.el
                        [ El.width <| El.px 300
                        , El.centerX
                        , El.above <|
                            El.el [ El.padding 50 ] <|
                                Inp.currentPassword
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
                      <|
                        El.none
                    ]
            ]


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


{-| focusedOnLoad doesn't work reliably, so use Dom.focus with this ID in addition
-}
nameTextFieldId : String
nameTextFieldId =
    "nameTextField"


{-| ibid
-}
msgTextAreaId : String
msgTextAreaId =
    "msgTextArea"


otherState : State -> State
otherState state =
    case state of
        Active ->
            Paused

        Paused ->
            Active


viewLoggedIn : LoggedInModel -> List (Html FrontendMsg)
viewLoggedIn model =
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

        effectiveState =
            case model.role of
                Student ->
                    model.state

                Teacher ->
                    Active

        viewHashOf msg =
            El.paragraph [ Font.family [ Font.monospace ] ] <|
                case splitLeadingZeros <| hash model.hashPrefixLen <| msg of
                    ( Nothing, remainder ) ->
                        [ El.text remainder ]

                    ( Just zeros, remainder ) ->
                        [ El.el
                            [ Bg.color darkishgray
                            , Font.color yellow
                            ]
                          <|
                            El.text zeros
                        , El.text remainder
                        ]
    in
    case model.name of
        Naming n ->
            [ fullScreenWhiteOnBlack <|
                El.row
                    [ El.centerX
                    , El.centerY
                    , El.spacingXY 10 0
                    ]
                    [ Inp.text
                        [ Bg.color black
                        , Font.color white
                        , Inp.focusedOnLoad
                        , El.width <| El.px 300
                        , onEnter AcceptName
                        , Inp.focusedOnLoad
                        , El.htmlAttribute <| Attr.id nameTextFieldId
                        ]
                        { onChange = UpdateName
                        , text = n
                        , placeholder = Just <| Inp.placeholder [] <| El.text "Your Name"
                        , label = Inp.labelHidden "Your Name"
                        }
                    , styledButton [] { label = El.text "⏎", onPress = Just AcceptName }
                    ]
            ]

        Named n ->
            viewNamed model n viewHashOf effectiveState disabled


hashDisplayWidth : Int
hashDisplayWidth =
    100


ifTeacher : LoggedInModel -> El.Element msg -> El.Element msg
ifTeacher model el =
    case model.role of
        Student ->
            El.none

        Teacher ->
            el


ifStudent : LoggedInModel -> El.Element msg -> El.Element msg
ifStudent model el =
    case model.role of
        Student ->
            el

        Teacher ->
            El.none


viewNamed : LoggedInModel -> String -> (String -> El.Element FrontendMsg) -> State -> Html.Attribute FrontendMsg -> List (Html FrontendMsg)
viewNamed model n viewHashOf effectiveState disabled =
    [ fullScreenWhiteOnBlack <|
        El.column
            [ El.width El.fill, El.height El.fill, El.spacing 20, El.padding 10 ]
        <|
            [ viewToolbar model n
            , El.el [ pausedStamp model, El.width El.fill ] <|
                viewMsgShare model viewHashOf effectiveState disabled
            , case model.messages of
                [] ->
                    El.none

                _ ->
                    viewMsgTable model.messages (msgTeacherActions model) viewHashOf
            , ifTeacher model <|
                viewMsgTable model.shareRequests (msgShareReqActions model) viewHashOf
            , ifStudent model <|
                El.el [ El.alignRight, El.alignBottom ] <|
                    Inp.button []
                        { onPress = Just ReLogin
                        , label = El.text "T"
                        }
            ]
    ]


pausedStamp : LoggedInModel -> El.Attribute msg
pausedStamp model =
    El.inFront <|
        case model.role of
            Teacher ->
                El.none

            Student ->
                case model.state of
                    Active ->
                        El.none

                    Paused ->
                        El.el
                            [ El.centerX
                            , El.centerY
                            , El.rotate <| degrees -30
                            , Border.color darkgray
                            , Border.width 2
                            , Border.rounded 7
                            , El.padding 5
                            , Border.dashed
                            ]
                        <|
                            El.text "One Two Three\nEyes On Me"


msgTeacherActions : LoggedInModel -> List (ColumnButton FrontendMsg)
msgTeacherActions model =
    case model.role of
        Student ->
            []

        Teacher ->
            [ { label = "X", header = "", msg = DeleteMessageFe } ]


msgShareReqActions : LoggedInModel -> List (ColumnButton FrontendMsg)
msgShareReqActions model =
    case model.role of
        Student ->
            []

        Teacher ->
            [ { label = "X", header = "", msg = DenyMessageFe }
            , { label = "OK", header = "", msg = PermitMessageFe }
            ]


viewToolbar : LoggedInModel -> String -> El.Element FrontendMsg
viewToolbar model n =
    El.row
        [ El.alignTop
        , El.width El.fill
        , El.height <| El.px 40
        , El.spacing 10
        ]
        [ case model.autoHashing of
            Disabled ->
                ifTeacher model <|
                    styledButton [] { onPress = Just EnableAutoHashFe, label = El.text "💻 On" }

            Enabled autoDigits ->
                El.row [ El.alignLeft, El.width El.fill ]
                    [ ifTeacher model <|
                        styledButton [] { onPress = Just DisableAutoHashFe, label = El.text "💻 Off" }
                    , ifStudent model <| El.text "Make it start with "
                    , numericInput UpdateAutoHashPrefixLen
                        (String.fromInt autoDigits)
                        "Automatic Hashing Digits"
                    , ifStudent model <| El.text " zeros "
                    , styledButton []
                        { onPress = Just AutoHash
                        , label = El.text "💻 Go!"
                        }
                    ]
        , ifTeacher model <|
            El.row [ El.alignRight, El.width El.shrink ]
                [ El.text "Show "
                , numericInput UpdatePrefixLenFe
                    (fromInt model.hashPrefixLen)
                    "Hash Prefix Length"
                , El.text " digits"
                ]
        , ifTeacher model <|
            styledButton []
                { onPress = Just <| ChangeStateFe <| otherState model.state
                , label =
                    El.text <|
                        case model.state of
                            Active ->
                                "Pause"

                            Paused ->
                                "Activate"
                }
        , ifTeacher model <|
            styledButton []
                { onPress = Just PushDraftMessageFe
                , label = El.text "Push"
                }
        , ifTeacher model <|
            styledButton []
                { onPress = Just ToggleAutoPush
                , label =
                    case model.autoPush of
                        AutoPushDisabled ->
                            El.text "+ AutoPush"

                        AutoPushEnabled ->
                            El.text "- AutoPush"
                }
        , ifTeacher model <|
            styledButton []
                { onPress = Just ClearMessagesFe
                , label = El.text "Clear"
                }
        , El.el [ El.alignRight, El.alignTop ]
            (El.text <| "👤 " ++ n)
        ]


numericInput : (String -> msg) -> String -> String -> El.Element msg
numericInput onChange text label =
    Inp.text
        [ Font.color white
        , Bg.color black
        , El.width <| El.px 60
        , El.htmlAttribute <| Attr.type_ "number"
        ]
        { onChange = onChange
        , text = text
        , placeholder = Nothing
        , label = Inp.labelHidden label
        }


viewMsgShare : LoggedInModel -> (String -> El.Element FrontendMsg) -> State -> Html.Attribute FrontendMsg -> El.Element FrontendMsg
viewMsgShare model viewHashOf effectiveState disabled =
    El.row
        [ El.spacingXY 20 0, El.width El.fill ]
    <|
        [ Inp.multiline
            [ Font.color <|
                case effectiveState of
                    Active ->
                        white

                    Paused ->
                        darkgray
            , Bg.color black
            , Font.family [ Font.monospace ]
            , Inp.focusedOnLoad
            , El.htmlAttribute disabled
            , El.htmlAttribute <| Attr.id msgTextAreaId
            ]
            { onChange = UpdateMessage
            , text = msgWithHashSuffix model
            , placeholder = Just <| Inp.placeholder [] <| El.text "Enter Your Message"
            , label = Inp.labelHidden "Enter Your Message"
            , spellcheck = False
            }
        , El.el [ El.width <| El.maximum hashDisplayWidth El.fill ] <|
            viewHashOf <|
                msgWithHashSuffix model
        , case effectiveState of
            Paused ->
                styledButton [ Border.color darkgray, Font.color darkgray ]
                    { onPress = Nothing
                    , label = El.text "Share"
                    }

            Active ->
                styledButton []
                    { onPress = Just ShareMessageFe
                    , label = El.text "Share"
                    }
        ]


type alias ColumnButton msg =
    { header : String
    , label : String
    , msg : String -> msg
    }


column : ColumnButton msg -> El.Column String msg
column cb =
    { header = El.text cb.header
    , width = El.shrink
    , view =
        \m ->
            styledButton []
                { label = El.text cb.label
                , onPress = Just <| cb.msg m
                }
    }


viewMsgTable : List String -> List (ColumnButton FrontendMsg) -> (String -> El.Element FrontendMsg) -> El.Element FrontendMsg
viewMsgTable messages colButtons viewHashOf =
    El.table [ El.height El.fill, El.scrollbarY ]
        { data = messages
        , columns =
            List.map column colButtons
                ++ [ { header = El.text "Message"
                     , width = El.fill
                     , view =
                        \m ->
                            El.el
                                [ Font.family [ Font.monospace ]
                                , El.paddingXY 0 5
                                ]
                            <|
                                El.paragraph [ El.htmlAttribute <| Attr.style "overflow-wrap" "anywhere" ]
                                    [ El.text m ]
                     }
                   , { header = El.text "Hash"
                     , width = El.maximum hashDisplayWidth El.fill
                     , view =
                        \m ->
                            El.el
                                [ El.paddingXY 0 5
                                , El.width <| El.maximum hashDisplayWidth El.fill
                                ]
                            <|
                                viewHashOf m
                     }
                   ]
        }

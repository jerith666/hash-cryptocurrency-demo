module Frontend exposing (app)

import Binary
import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html exposing (Html)
import Html.Attributes as Attr
import Html.Events
import Lamdera exposing (sendToBackend)
import SHA
import String exposing (fromInt)
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

        NoOpFrontendMsg ->
            ( model, Cmd.none )


unexpected _ model =
    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    let
        initModel beModel shareRequests role =
            { message = ""
            , hashPrefixLen = beModel.hashPrefixLen
            , binaryDigits = Three
            , messages = beModel.messages
            , shareRequests = shareRequests
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

                LoggedIn _ ->
                    unexpected msg model

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
                [ Attr.value model.message
                , Attr.rows 5
                , Attr.cols 80
                , Html.Events.onInput UpdateMessage
                ]
                []

        prefixSpinner =
            Html.input
                [ Attr.type_ "number"
                , Attr.min "1"
                , Attr.max "64"
                , Attr.value <| fromInt model.hashPrefixLen
                , Html.Events.onInput UpdatePrefixLenFe
                ]
                []

        msgHash =
            Html.div
                [ Attr.style "font-family" "monospace" ]
                [ Html.text <| hashFn model.message ]

        shareButton =
            Html.button [ Html.Events.onClick ShareMessageFe ] [ Html.text "Share Message" ]

        shareDecision message =
            [ Html.td [] [ Html.button [ Html.Events.onClick <| PermitMessageFe message ] [ Html.text "permit" ] ]
            , Html.td [] [ Html.button [ Html.Events.onClick <| DenyMessageFe message ] [ Html.text "deny" ] ]
            ]

        msgsTable messages shareMaker =
            Html.table [] <|
                [ Html.tr [] <| List.map (\t -> Html.td [] [ Html.text t ]) [ "message", "hash" ] ]
                    ++ List.map
                        (\m ->
                            Html.tr [] <|
                                [ Html.td [] [ Html.text m ]
                                , Html.td [ Attr.style "font-family" "monospace" ] [ Html.text <| hashFn m ]
                                ]
                                    ++ shareMaker m
                        )
                        messages
    in
    [ Html.div [] <|
        case model.role of
            Teacher ->
                [ msgArea, prefixSpinner, msgHash, shareButton, msgsTable model.messages <| always [], msgsTable model.shareRequests shareDecision ]

            Student ->
                [ msgArea, msgHash, shareButton, msgsTable model.messages <| always [] ]
    ]

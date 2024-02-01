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

        UpdatePrefixLen newLenStr ->
            case model of
                AnonFrontend _ _ ->
                    unexpected msg model

                LoggedIn m ->
                    case String.toInt newLenStr of
                        Nothing ->
                            ( model, Cmd.none )

                        Just newLen ->
                            ( LoggedIn { m | hashPrefixLen = newLen }, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


unexpected _ model =
    ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        TeacherLoginOk ->
            case model of
                AnonFrontend _ _ ->
                    ( LoggedIn
                        { message = ""
                        , hashPrefixLen = 1
                        , binaryDigits = Three
                        , messages = []
                        , role = Teacher
                        }
                    , Cmd.none
                    )

                LoggedIn _ ->
                    unexpected msg model

        TeacherLoginBad ->
            case model of
                AnonFrontend password _ ->
                    ( AnonFrontend password LoginFailed, Cmd.none )

                LoggedIn _ ->
                    unexpected msg model

        TeacherArrived ->
            case model of
                AnonFrontend _ state ->
                    case state of
                        LoginInProgress ->
                            unexpected msg model

                        _ ->
                            ( LoggedIn
                                { message = ""
                                , hashPrefixLen = 1
                                , binaryDigits = Three
                                , messages = []
                                , role = Student
                                }
                            , Cmd.none
                            )

                LoggedIn _ ->
                    unexpected msg model


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
    in
    [ Html.div []
        [ Html.text <|
            case model.role of
                Teacher ->
                    "teacher"

                Student ->
                    "student"
        , Html.textarea
            [ Attr.value model.message
            , Attr.rows 10
            , Attr.cols 120
            , Html.Events.onInput UpdateMessage
            ]
            []
        , Html.input
            [ Attr.type_ "range"
            , Attr.min "1"
            , Attr.max "64"
            , Attr.value <| fromInt model.hashPrefixLen
            , Html.Events.onInput UpdatePrefixLen
            ]
            []
        , Html.div
            [ Attr.style "font-family" "monospace" ]
            [ Html.text <| hashFn model.message ]
        , Html.table [] <|
            [ Html.tr [] <| List.map (\t -> Html.td [] [ Html.text t ]) [ "message", "hash" ] ]
                ++ List.map
                    (\m ->
                        Html.tr []
                            [ Html.td [] [ Html.text m ]
                            , Html.td [ Attr.style "font-family" "monospace" ] [ Html.text <| hashFn m ]
                            ]
                    )
                    model.messages
        ]
    ]

module Frontend exposing (..)

import Browser exposing (UrlRequest(..))
import Browser.Navigation as Nav
import Html
import Html.Attributes as Attr
import Html.Events
import Lamdera
import SHA256
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
    ( { message = ""
      , hashPrefixLen = 1
      }
    , Cmd.none
    )


update : FrontendMsg -> Model -> ( Model, Cmd FrontendMsg )
update msg model =
    case msg of
        UpdateMessage newMsg ->
            ( { model | message = newMsg }, Cmd.none )

        UpdatePrefixLen newLenStr ->
            case String.toInt newLenStr of
                Nothing ->
                    ( model, Cmd.none )

                Just newLen ->
                    ( { model | hashPrefixLen = newLen }, Cmd.none )

        NoOpFrontendMsg ->
            ( model, Cmd.none )


updateFromBackend : ToFrontend -> Model -> ( Model, Cmd FrontendMsg )
updateFromBackend msg model =
    case msg of
        NoOpToFrontend ->
            ( model, Cmd.none )


view : Model -> Browser.Document FrontendMsg
view model =
    let
        hash =
            String.left model.hashPrefixLen <|
                SHA256.toHex <|
                    SHA256.fromString model.message
    in
    { title = "Hash and Cryptocurrency Demo"
    , body =
        [ Html.div []
            [ Html.textarea
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
                [ Html.text hash ]
            ]
        ]
    }

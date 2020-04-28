module Main exposing (Model, init, Msg, update, view, subscriptions)

import Html exposing (..)
import Browser
import Browser.Navigation as Nav
import Url
import Maybe exposing (Maybe)
import Discourse
import Dict exposing (Dict)


main : Program flags Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }

type ReaderState =
    Error String
    | Reading
        { activeTopic : Discourse.Id
        , topics : Dict Discourse.Id Discourse.Topic
        }

type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , state : ReaderState
    }


init : flags -> Url.Url -> Nav.Key -> (Model, Cmd Msg)
init _ url key =
    ({ key = key
    , url = url
    , topic = Nothing
    }, Cmd.none)


type Msg
    = 
    UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url


update : Msg -> Model -> (Model, Cmd Msg)
update msg model =
    case msg of
        UrlRequested urlRequest ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    { title = "Application Title"
    , body =
        [ div []
            [ text "New Application" ]
      ]
    }



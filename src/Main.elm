module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Discourse
import Html exposing (..)
import Http
import Json.Decode as D
import Maybe exposing (Maybe)
import Url
import Url.Parser as P exposing ((</>))


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = UrlRequested
        , onUrlChange = UrlChanged
        }


type alias ReaderState =
    { baseUrl : Url.Url
    , activeTopic : Int
    , topics : Dict Int Discourse.Topic
    }


type PageState
    = Error String
    | Loading Url.Url (Maybe ReaderState)
    | Reader ReaderState


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , state : PageState
    }


type Route
    = ReadTopic Discourse.TopicId (Maybe Int)
    | NotFound


badJson : String
badJson =
    """
I can't understand the topic that was returned from the server. Please contact someone who developed me to fix this!
"""


route =
    let
        idOrSlug =
            P.oneOf [ P.map Discourse.Id P.int, P.map Discourse.Slug P.string ]
                |> P.map (Debug.log "idOrSlug")

        optionalId =
            P.custom "POSTID" (Just << String.toInt)
                |> P.map (Debug.log "optionalId")
    in
    P.oneOf
        [ P.map (\tid -> ReadTopic tid Nothing) (P.s "t" </> idOrSlug)
        , P.map ReadTopic (P.s "t" </> idOrSlug </> optionalId)
        ]


toRoute : Url.Url -> Route
toRoute url =
    Maybe.withDefault NotFound (P.parse route url)


init : D.Value -> Url.Url -> Nav.Key -> ( Model, Cmd Msg )
init flags url key =
    let
        badUrl =
            "I need an object with the key \"discourseUrl\" containing a valid URL to a discourse instance passed to my init function."

        serverUrl =
            D.decodeValue (D.field "discourseUrl" D.string) flags
                |> Result.mapError (\_ -> badUrl)
                |> Result.map Url.fromString
                |> Result.andThen (Result.fromMaybe badUrl)

        page =
            toRoute url
                |> Debug.log "route"

        ( state, cmd ) =
            case ( serverUrl, page ) of
                ( Ok discourseUrl, ReadTopic tid pid ) ->
                    ( Loading discourseUrl Nothing
                    , Discourse.fetchTopic discourseUrl tid GotTopic
                    )

                ( Ok _, NotFound ) ->
                    ( Error "Topic not found.", Cmd.none )

                ( Err msg, _ ) ->
                    ( Error msg, Cmd.none )
    in
    ( { key = key
      , url = url
      , state = state
      }
    , cmd
    )


type Msg
    = UrlRequested Browser.UrlRequest
    | UrlChanged Url.Url
    | GotTopic (Result Http.Error ( Int, Discourse.Topic ))


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.state, msg ) of
        ( _, UrlRequested urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    ( model, Nav.pushUrl model.key (Url.toString url) )

                Browser.External href ->
                    ( model, Nav.load href )

        ( _, UrlChanged url ) ->
            ( { model | url = url }
            , Cmd.none
            )

        ( Loading discourseUrl prevState, GotTopic (Ok ( tid, topic )) ) ->
            ( { model
                | state =
                    Reader
                        { baseUrl = discourseUrl
                        , activeTopic = tid
                        , topics = Dict.singleton tid topic
                        }
              }
            , Cmd.none
            )

        ( Reader state, GotTopic (Ok ( tid, topic )) ) ->
            ( { model | state = Reader { state | topics = Dict.insert tid topic state.topics } }, Cmd.none )

        ( _, GotTopic (Err e) ) ->
            let
                _ =
                    case e of
                        Http.BadBody s ->
                            Debug.log "Bad json response" s

                        _ ->
                            ""
            in
            ( { model | state = Error badJson }, Cmd.none )

        ( Error _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    let
        title =
            Maybe.withDefault "Brreader" <|
                case model.state of
                    Reader r ->
                        Dict.get r.activeTopic r.topics
                            |> Maybe.map .title

                    _ ->
                        Nothing
    in
    { title = title
    , body =
        case model.state of
            Error s ->
                [ text s
                ]

            Loading url _ ->
                [ text ("Loading " ++ Url.toString url)
                ]

            Reader r ->
                [ text (String.fromInt r.activeTopic)
                ]
    }

module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Discourse
import Html as H
import Html.Attributes as A
import Html.Parser.Util as HtmlUtil
import Http
import Json.Decode as D
import Maybe exposing (Maybe)
import Set
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
    , forkRoot : Int
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


route : P.Parser (Route -> a) a
route =
    let
        idOrSlug =
            P.oneOf [ P.map Discourse.Id P.int, P.map Discourse.Slug P.string ]

        optionalId =
            P.custom "POSTID" (Just << String.toInt)
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

        ( state, cmd ) =
            case ( serverUrl, page ) of
                ( Ok srvUrl, ReadTopic tid pnr ) ->
                    ( Loading srvUrl Nothing
                    , Discourse.fetchTopic srvUrl tid Nothing GotTopic
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

        ( Loading srvUrl prevState, GotTopic (Ok ( newTopicId, newTopic )) ) ->
            let
                state =
                    case prevState of
                        Just s ->
                            { s | topics = Dict.insert newTopicId newTopic s.topics, forkRoot = newTopicId }

                        Nothing ->
                            ReaderState srvUrl newTopicId newTopicId (Dict.singleton newTopicId newTopic)

                isCircularDep visited id =
                    let
                        isVisited =
                            Set.member id visited
                    in
                    case ( isVisited, Discourse.parentTopicAndPostId id state.topics ) of
                        ( True, _ ) ->
                            True

                        ( False, Nothing ) ->
                            False

                        ( False, Just ( parentTopicId, _ ) ) ->
                            isCircularDep (Set.insert id visited) parentTopicId
            in
            case ( isCircularDep Set.empty newTopicId, Discourse.parentTopicAndPostId newTopicId state.topics ) of
                ( False, Just ( parentTopicId, parentPostNr ) ) ->
                    ( { model | state = Loading srvUrl (Just state) }
                    , Discourse.fetchTopic srvUrl (Discourse.Id parentTopicId) (Just ( parentPostNr, newTopicId )) GotTopic
                    )

                _ ->
                    ( { model | state = Reader state }, Cmd.none )

        ( Reader state, GotTopic (Ok ( tid, topic )) ) ->
            ( { model | state = Reader { state | topics = Dict.insert tid topic state.topics } }, Cmd.none )

        ( _, GotTopic (Err _) ) ->
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
                [ H.text s
                ]

            Loading _ _ ->
                [ H.text "Loading"
                ]

            Reader r ->
                let
                    post1 =
                        Dict.get r.forkRoot r.topics
                            |> Maybe.andThen (Discourse.getPost 1)

                    thread visited p =
                        case ( Set.member ( p.topicId, p.seq ) visited, Discourse.nextPost p r.topics ) of
                            ( False, Just n ) ->
                                p :: thread (Set.insert ( p.topicId, p.seq ) visited) n

                            _ ->
                                []

                    domId p =
                        "post-" ++ String.fromInt p.topicId ++ "-" ++ String.fromInt p.seq

                    viewPost p =
                        H.div [ A.id (domId p) ] (HtmlUtil.toVirtualDom p.body)
                in
                case post1 of
                    Just p1 ->
                        thread Set.empty p1
                            |> List.map viewPost

                    Nothing ->
                        []
    }

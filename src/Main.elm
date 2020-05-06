module Main exposing (Model, Msg, init, subscriptions, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Discourse
import Html as H
import Html.Attributes as A
import Html.Events as E
import Html.Parser.Util as HtmlUtil
import Http
import Json.Decode as D
import Maybe exposing (Maybe)
import Parser as P exposing ((|.), (|=))
import Set
import Task
import Url


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
    | Loading Url.Url (Maybe ReaderState) (Maybe ( Int, Int ))
    | Reader ReaderState


type alias Model =
    { key : Nav.Key
    , url : Url.Url
    , state : PageState
    }


type Route
    = ReadTopic Discourse.TopicId (Maybe ( Int, Int ))
    | NotFound


badJson : String
badJson =
    """
I can't understand the topic that was returned from the server. Please contact someone who developed me to fix this!
"""


toRoute : Url.Url -> Route
toRoute url =
    let
        segmentStr =
            P.getChompedString <|
                P.succeed ()
                    |. P.chompUntilEndOr "/"

        idOrSlug =
            P.oneOf [ P.map Discourse.Id P.int, P.map Discourse.Slug segmentStr ]

        idsOrNothing =
            P.oneOf
                [ P.succeed Tuple.pair
                    |. P.symbol "/"
                    |= P.int
                    |. P.symbol "/"
                    |= P.int
                    |> P.map Just
                , P.succeed Nothing
                ]

        route =
            P.succeed ReadTopic
                |. P.chompIf (\c -> c == '/')
                |. P.symbol "t/"
                |= idOrSlug
                |= idsOrNothing
                |. P.end
    in
    case P.run route url.path of
        Ok r ->
            r

        _ ->
            url.fragment
                |> Maybe.map (P.run route)
                |> Maybe.andThen Result.toMaybe
                |> Maybe.withDefault NotFound


discourseUrl : PageState -> Maybe Url.Url
discourseUrl s =
    case s of
        Loading url _ _ ->
            Just url

        Reader r ->
            Just r.baseUrl

        _ ->
            Nothing


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
                ( Ok srvUrl, ReadTopic tid showPost ) ->
                    ( Loading srvUrl Nothing showPost
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
    | SetActiveFork ( Int, Int ) (Maybe ( Int, Int ))
    | NoOp


scrollToPost : Int -> Int -> Cmd Msg
scrollToPost topicId postNr =
    Dom.getElement (domId topicId postNr)
        |> Task.andThen (\e -> Dom.setViewport e.element.x e.element.y)
        |> Task.attempt (\_ -> NoOp)


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case ( model.state, msg ) of
        ( _, NoOp ) ->
            ( model, Cmd.none )

        ( Loading _ _ _, SetActiveFork _ _ ) ->
            ( model, Cmd.none )

        ( _, UrlRequested urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    case toRoute url of
                        NotFound ->
                            ( model, Cmd.none )

                        ReadTopic slugOrId scrollTo ->
                            ( model
                            , scrollTo
                                |> Maybe.map
                                    (\( topicId, postNr ) ->
                                        [ scrollToPost topicId postNr
                                        , Nav.pushUrl model.key (Url.toString url)
                                        ]
                                    )
                                |> Maybe.withDefault []
                                |> Cmd.batch
                            )

                --( model, Nav.pushUrl model.key (Url.toString url) )
                Browser.External href ->
                    case model.state of
                        Reader state ->
                            case Discourse.topicAndPostIdFromUrl state.baseUrl href of
                                (Just ( topicId, _ )) as fork ->
                                    ( { model | state = Loading state.baseUrl (Just state) fork }
                                    , Discourse.fetchTopic state.baseUrl (Discourse.Id topicId) Nothing GotTopic
                                    )

                                Nothing ->
                                    ( model, Nav.load href )

                        _ ->
                            ( model, Nav.load href )

        ( _, UrlChanged url ) ->
            ( { model | url = url }
            , Cmd.none
            )

        ( Loading srvUrl prevState scrollTo, GotTopic (Ok ( newTopicId, newTopic )) ) ->
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
                    ( { model | state = Loading srvUrl (Just state) scrollTo }
                    , Discourse.fetchTopic srvUrl (Discourse.Id parentTopicId) (Just ( parentPostNr, newTopicId )) GotTopic
                    )

                _ ->
                    ( { model | state = Reader state }
                    , case scrollTo of
                        Just ( topicId, postNr ) ->
                            scrollToPost topicId postNr

                        _ ->
                            Cmd.none
                    )

        ( Reader state, GotTopic (Ok ( tid, topic )) ) ->
            ( { model | state = Reader { state | topics = Dict.insert tid topic state.topics } }, Cmd.none )

        ( Reader state, SetActiveFork ( onTopicId, onPostNr ) selectedFork ) ->
            let
                newState =
                    { state
                        | topics =
                            state.topics
                                |> Dict.update onTopicId
                                    (Maybe.map
                                        (\topic ->
                                            { topic
                                                | posts =
                                                    topic.posts
                                                        |> Dict.update onPostNr
                                                            (Maybe.map
                                                                (\post ->
                                                                    { post | activeFork = selectedFork }
                                                                )
                                                            )
                                            }
                                        )
                                    )
                    }
            in
            ( { model | state = Reader newState }, Cmd.none )

        ( _, GotTopic (Err _) ) ->
            ( { model | state = Error badJson }, Cmd.none )

        ( Error _, _ ) ->
            ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


domId : Int -> Int -> String
domId topicId postNr =
    "post-" ++ String.fromInt topicId ++ "-" ++ String.fromInt postNr


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

            Loading _ _ _ ->
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

                    postForkSelector p =
                        if not (List.isEmpty p.forks) then
                            p.forks
                                |> List.indexedMap
                                    (\i fork ->
                                        H.a [ E.onClick (SetActiveFork ( p.topicId, p.seq ) (Just fork)) ] [ H.text ("fork" ++ String.fromInt (i + 1)) ]
                                    )
                                |> List.append [ H.a [ E.onClick (SetActiveFork ( p.topicId, p.seq ) Nothing) ] [ H.text "original" ] ]
                                |> H.div []

                        else
                            H.div [] []

                    viewPost p =
                        [ postForkSelector p
                        , H.div [ A.id (domId p.topicId p.seq) ] (HtmlUtil.toVirtualDom p.body)
                        ]
                in
                case post1 of
                    Just p1 ->
                        thread Set.empty p1
                            |> List.concatMap viewPost

                    Nothing ->
                        []
    }

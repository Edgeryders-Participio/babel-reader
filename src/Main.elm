port module Main exposing (Model, Msg, init, subscriptions, update, view)

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
import Set exposing (Set)
import Task
import Url
import Url.Builder as B


port setBodyClass : String -> Cmd msg


applyBodyClass : ReaderState -> Cmd msg
applyBodyClass state =
    if state.isLight then
        setBodyClass "yin"

    else
        setBodyClass "yang"


main : Program D.Value Model Msg
main =
    Browser.application
        { init = init
        , view = view
        , update = update
        , subscriptions = subscriptions
        , onUrlRequest = LinkClicked
        , onUrlChange = UrlChanged
        }


type alias ReaderState =
    { baseUrl : Url.Url
    , forkRoot : Int
    , activeTopic : Int
    , topics : Dict Int Discourse.Topic
    , showAuthor : Set ( Int, Int )
    , isLight : Bool
    }


type PageState
    = Error String
    | LoadingThread Url.Url (Maybe ReaderState) (Maybe ( Int, Int ))
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
        tokenOpt t =
            P.oneOf [ P.token t, P.succeed () ]

        id =
            P.chompWhile Char.isDigit
                |> P.getChompedString
                |> P.andThen
                    (\s ->
                        case String.toInt s of
                            Just i ->
                                P.succeed i

                            _ ->
                                P.problem ("\"" ++ s ++ "\" is not a valid id.")
                    )

        segmentStr =
            P.getChompedString <|
                P.succeed ()
                    |. P.chompUntilEndOr "/"

        idOrSlug =
            P.oneOf
                [ P.map Discourse.Id id
                , P.map Discourse.Slug segmentStr
                ]

        idsOrNothing =
            P.oneOf
                [ P.succeed Tuple.pair
                    |. P.backtrackable (P.token "/")
                    |= id
                    |. P.token "/"
                    |= id
                    |> P.map Just
                , P.succeed Nothing
                ]

        route =
            P.succeed ReadTopic
                |. tokenOpt "/"
                |. P.token "t/"
                |= idOrSlug
                |= idsOrNothing
                |. tokenOpt "/"
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
                    ( LoadingThread srvUrl Nothing showPost
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
    , Cmd.batch
        [ cmd
        , setBodyClass "yin"
        ]
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | GotTopic (Result Http.Error ( Int, Discourse.Topic ))
    | SetActiveFork Int Int (Maybe Int)
    | SetShowAuthor Int Int Bool
    | ToggleYinYang
    | NoOp


scrollToPost : Int -> Int -> Cmd Msg
scrollToPost topicId postNr =
    Dom.getElement (domId topicId postNr)
        |> Task.andThen (\e -> Dom.setViewport e.element.x e.element.y)
        |> Task.attempt (\_ -> NoOp)


switchToTopic : Int -> Maybe ( Int, Int ) -> PageState -> ( PageState, Cmd Msg )
switchToTopic topicId scrollTo state =
    ( state, Cmd.none )


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    let
        _ =
            1

        --switchToTopic topicId =
    in
    case ( model.state, msg ) of
        ( _, LinkClicked urlRequest ) ->
            case urlRequest of
                Browser.Internal url ->
                    case toRoute url of
                        NotFound ->
                            ( model, Nav.load (Url.toString url) )

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

                Browser.External href ->
                    case model.state of
                        Reader state ->
                            case Discourse.topicAndPostIdFromUrl state.baseUrl href of
                                (Just ( topicId, _ )) as fork ->
                                    ( { model | state = LoadingThread state.baseUrl (Just state) fork }
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

        ( LoadingThread srvUrl prevState scrollTo, GotTopic (Ok ( newTopicId, newTopic )) ) ->
            let
                state =
                    case prevState of
                        Just s ->
                            { s | topics = Dict.insert newTopicId newTopic s.topics, forkRoot = newTopicId }

                        Nothing ->
                            ReaderState srvUrl newTopicId newTopicId (Dict.singleton newTopicId newTopic) Set.empty False

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
                    ( { model | state = LoadingThread srvUrl (Just state) scrollTo }
                    , Discourse.fetchTopic srvUrl (Discourse.Id parentTopicId) (Just ( parentPostNr, newTopicId )) GotTopic
                    )

                _ ->
                    ( { model | state = Reader state }
                    , case scrollTo of
                        Just ( topicId, postNr ) ->
                            scrollToPost topicId postNr

                        _ ->
                            scrollToPost state.activeTopic 1
                    )

        ( Reader state, GotTopic (Ok ( tid, topic )) ) ->
            let
                topics =
                    Dict.insert tid topic state.topics

                affectedPost : Maybe Discourse.Post
                affectedPost =
                    Discourse.getPost 1 topic
                        |> Maybe.andThen .parent
                        |> Maybe.andThen (\( topicId, postNr ) -> Dict.get topicId state.topics |> Maybe.andThen (Discourse.getPost postNr))

                newTopics =
                    affectedPost
                        |> Maybe.map (\p -> Discourse.updatePost p.topicId p.seq (Discourse.verifyForks topics) topics)
                        |> Maybe.withDefault topics
            in
            ( { model
                | state =
                    Reader
                        { state
                            | topics = newTopics
                        }
              }
            , Cmd.none
            )

        ( Reader state, SetActiveFork onTopicId onPostNr selectedForkTopicId ) ->
            ( { model
                | state =
                    Reader
                        { state
                            | topics = Discourse.updatePost onTopicId onPostNr (Discourse.setActiveFork selectedForkTopicId) state.topics
                        }
              }
            , Cmd.none
              --scrollToPost onTopicId onPostNr
            )

        ( Reader state, SetShowAuthor onTopicId onPostNr show ) ->
            let
                newShowAuthors =
                    if show then
                        Set.insert ( onTopicId, onPostNr ) state.showAuthor

                    else
                        Set.remove ( onTopicId, onPostNr ) state.showAuthor
            in
            ( { model | state = Reader { state | showAuthor = newShowAuthors } }, Cmd.none )

        ( Reader state, ToggleYinYang ) ->
            ( { model | state = Reader { state | isLight = not state.isLight } }
            , applyBodyClass state
            )

        ( _, GotTopic (Err _) ) ->
            ( { model | state = Error badJson }, Cmd.none )

        _ ->
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
            Maybe.withDefault "Babel Between Us Reader" <|
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
                [ H.section [] [ H.text s ]
                , H.footer [] []
                ]

            LoadingThread _ _ _ ->
                [ H.section [] [ H.div [ A.class "lds-dual-ring" ] [] ]
                , H.footer [] []
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
                                [ p ]

                    forkHref : Int -> String
                    forkHref fromTopicId =
                        Dict.get fromTopicId r.topics
                            |> Maybe.map .slug
                            |> Maybe.map (\slug -> B.relative [ "#", "t", slug, String.fromInt fromTopicId, "1" ] [])
                            |> Maybe.withDefault ""

                    onClickLink msg =
                        E.custom "click" (D.succeed { message = msg, stopPropagation = True, preventDefault = True })

                    forkLink : Discourse.Post -> Int -> String -> H.Html Msg
                    forkLink p fork text =
                        let
                            active =
                                p.activeFork
                                    |> Maybe.map ((==) fork)
                                    |> Maybe.withDefault False

                            forkTitle =
                                Dict.get fork r.topics
                                    |> Maybe.map .title
                                    |> Maybe.withDefault (String.fromInt fork)

                            ( setTo, tooltip ) =
                                if not active then
                                    ( Just fork, "Show fork '" ++ forkTitle ++ "'" )

                                else
                                    ( Nothing, "Don't show a fork" )
                        in
                        H.a
                            [ A.target "_self"
                            , A.href (forkHref fork)
                            , A.title tooltip
                            , A.classList [ ( "active", active ) ]
                            , onClickLink (SetActiveFork p.topicId p.seq setTo)
                            ]
                            [ H.text text
                            ]

                    postForkSelector p =
                        if not (List.isEmpty p.forks) then
                            let
                                forkHelp =
                                    "Click on a number to shpw that fork. You can click on a selected fork again to see the original topic."

                                linkList =
                                    p.forks
                                        |> List.map Discourse.forkTopicId
                                        |> List.indexedMap (\i fromTopicId -> forkLink p fromTopicId (String.fromInt (i + 1)))
                            in
                            H.div
                                [ A.class "fork-selector" ]
                                [ H.div [] [ H.span [ A.class "icon-fork", A.title forkHelp ] [] ]
                                , H.div [ A.class "fork-list" ] linkList
                                ]

                        else
                            H.div [] []

                    viewAuthor p visible =
                        H.div
                            [ A.class "author"
                            , if visible then
                                A.class "visible"

                              else
                                A.class ""
                            ]
                            [ H.text (Maybe.withDefault p.username p.name) ]

                    viewPost p =
                        let
                            showAuthor =
                                Set.member ( p.topicId, p.seq ) r.showAuthor
                        in
                        [ H.article
                            [ A.id (domId p.topicId p.seq)
                            , E.onClick (SetShowAuthor p.topicId p.seq (not showAuthor))
                            ]
                            (viewAuthor p showAuthor :: HtmlUtil.toVirtualDom p.body)
                        , postForkSelector p
                        ]
                in
                case post1 of
                    Just p1 ->
                        [ H.span [ A.class "icon-yinyang", E.onClick ToggleYinYang ] []
                        , H.section [] (List.concatMap viewPost (thread Set.empty p1))
                        , H.footer [] []
                        ]

                    Nothing ->
                        []
    }

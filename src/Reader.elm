port module Reader exposing (Model, Msg, Theme(..), applyTheme, hasFinishedLoading, init, scrollToPost, selectForksForTopic, topicTitle, update, view)

import Browser
import Browser.Dom as Dom
import Browser.Navigation as Nav
import Dict exposing (Dict)
import Discourse
import Html as H exposing (Html)
import Html.Attributes as A
import Html.Events as E
import Html.Parser.Util as HtmlUtil
import Http
import Json.Decode as D
import Maybe exposing (Maybe)
import Maybe.Extra as MaybeEx
import Parser as P exposing ((|.), (|=))
import Set exposing (Set)
import Task
import Url
import Url.Builder as B


port setBodyClass : String -> Cmd msg


type Theme
    = Yin
    | Yang


applyTheme : Theme -> Cmd msg
applyTheme theme =
    case theme of
        Yin ->
            setBodyClass "yin"

        Yang ->
            setBodyClass "yang"


type Model msg
    = Model (State msg)


type alias State msg =
    { toMsg : Msg -> msg
    , baseUrl : Url.Url
    , topics : Dict Int Discourse.Topic
    , showAuthor : Set ( Int, Int )
    , theme : Theme
    }


type Msg
    = GotTopic (Result Http.Error ( Int, Discourse.Topic, List Int ))
    | GotTopicPosts ( Int, Discourse.Topic ) (Result Http.Error ( Int, List Discourse.Post ))
    | SetActiveFork Int Int (Maybe Int)
    | SetShowAuthor Int Int Bool
    | ToggleYinYang
    | NoOp


hasFinishedLoading : Model msg -> Discourse.TopicId -> Maybe Int
hasFinishedLoading (Model model) topicIdOrSlug =
    Discourse.topicIdFromSlug model.topics topicIdOrSlug
        |> Maybe.andThen
            (\topicId ->
                if Discourse.firstUnavailableParentTopicId model.topics topicId == Nothing then
                    Just topicId

                else
                    Nothing
            )


scrollToPost : Model msg -> Int -> Int -> Cmd msg
scrollToPost (Model model) topicId postNr =
    Dom.getElement (domId topicId postNr)
        |> Task.andThen (\e -> Dom.setViewport e.element.x e.element.y)
        |> Task.attempt (\_ -> NoOp)
        |> Cmd.map model.toMsg


topicTitle : Model msg -> Int -> Maybe String
topicTitle (Model model) topicId =
    Dict.get topicId model.topics
        |> Maybe.map .title


domId : Int -> Int -> String
domId topicId postNr =
    "post-" ++ String.fromInt topicId ++ "-" ++ String.fromInt postNr


mapModelCmd : ( State msg, Cmd Msg ) -> ( Model msg, Cmd msg )
mapModelCmd ( state, cmd ) =
    ( Model state, Cmd.map state.toMsg cmd )


init : Url.Url -> (Msg -> msg) -> Discourse.TopicId -> ( Model msg, Cmd msg )
init serverUrl toMsg topicId =
    let
        model =
            State toMsg serverUrl Dict.empty Set.empty Yin

        cmd =
            Discourse.fetchTopic serverUrl topicId GotTopic
                |> Cmd.map toMsg
    in
    ( Model model, cmd )


selectForksForTopic : Model msg -> Int -> Model msg
selectForksForTopic (Model model) topicId =
    let
        f visited id topics =
            let
                setActiveFork =
                    Discourse.setActiveFork (Just id)

                parent =
                    Dict.get id topics
                        |> Maybe.andThen Discourse.getFirstPost
                        |> Maybe.andThen .parent
            in
            case ( Set.member id visited, parent ) of
                ( False, Just ( parentTopicId, parentPostNr ) ) ->
                    f (Set.insert parentTopicId visited) parentTopicId (Discourse.updatePost parentTopicId parentPostNr setActiveFork topics)

                _ ->
                    { model | topics = topics }
    in
    Model (f Set.empty topicId model.topics)


finalizeReceivedTopic topics baseUrl newTopicId newTopic =
    let
        newTopics =
            Dict.insert newTopicId newTopic topics
                |> Discourse.verifyForks

        unavailableParent =
            Discourse.firstUnavailableParentTopicId newTopics newTopicId

        nextTopicId =
            MaybeEx.orLazy
                unavailableParent
                (\() -> Discourse.firstUnverifiedForkTopicId newTopics |> Maybe.map Tuple.second)

        cmd =
            nextTopicId
                |> Maybe.map (\topicId -> Discourse.fetchTopic baseUrl (Discourse.Id topicId) GotTopic)
                |> Maybe.withDefault Cmd.none
    in
    ( newTopics, cmd )


update : Msg -> Model msg -> ( Model msg, Cmd msg )
update msg (Model model) =
    mapModelCmd <|
        case msg of
            GotTopicPosts ( topicId, topic ) (Ok ( _, posts )) ->
                let
                    ( newTopics, cmd ) =
                        finalizeReceivedTopic
                            model.topics
                            model.baseUrl
                            topicId
                            (Discourse.addPosts topic posts)
                in
                ( { model | topics = newTopics }, cmd )

            GotTopic (Ok ( topicId, topic, missingPostIds )) ->
                let
                    ( newTopics, cmd ) =
                        if List.isEmpty missingPostIds then
                            finalizeReceivedTopic model.topics model.baseUrl topicId topic

                        else
                            ( model.topics
                            , Discourse.fetchPosts
                                model.baseUrl
                                topicId
                                missingPostIds
                                (GotTopicPosts ( topicId, topic ))
                            )
                in
                ( { model | topics = newTopics }, cmd )

            SetActiveFork onTopicId onPostNr selectedForkTopicId ->
                ( { model
                    | topics = Discourse.updatePost onTopicId onPostNr (Discourse.setActiveFork selectedForkTopicId) model.topics
                  }
                , Cmd.none
                )

            SetShowAuthor onTopicId onPostNr show ->
                let
                    newShowAuthors =
                        if show then
                            Set.insert ( onTopicId, onPostNr ) model.showAuthor

                        else
                            Set.remove ( onTopicId, onPostNr ) model.showAuthor
                in
                ( { model | showAuthor = newShowAuthors }, Cmd.none )

            ToggleYinYang ->
                let
                    newTheme =
                        case model.theme of
                            Yin ->
                                Yang

                            Yang ->
                                Yin
                in
                ( { model
                    | theme = newTheme
                  }
                , applyTheme newTheme
                )

            GotTopic (Err _) ->
                ( model, Cmd.none )

            GotTopicPosts _ (Err _) ->
                ( model, Cmd.none )

            NoOp ->
                ( model, Cmd.none )


view : Model msg -> Int -> List (Html msg)
view (Model r) topicId =
    let
        topic =
            Dict.get topicId r.topics

        post1 =
            Discourse.getRoot r.topics topicId
                |> Maybe.andThen Discourse.getFirstPost

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
                |> Maybe.map (\slug -> B.relative [ "#", "t", slug, String.fromInt fromTopicId ] [])
                |> Maybe.withDefault ""

        onClickLink msg =
            E.custom "click" (D.succeed { message = r.toMsg msg, stopPropagation = True, preventDefault = True })

        forkLink : Discourse.Post -> Maybe Int -> String -> H.Html msg
        forkLink p fork text =
            let
                active =
                    p.activeFork == fork

                ( href, forkTitle ) =
                    case fork of
                        Just fromTopicId ->
                            ( forkHref fromTopicId
                            , Dict.get fromTopicId r.topics
                                |> Maybe.map .title
                                |> Maybe.withDefault (String.fromInt fromTopicId)
                            )

                        _ ->
                            ( forkHref p.topicId, "<original>" )

                tooltip =
                    "Show fork '" ++ forkTitle ++ "'"
            in
            H.a
                [ A.target "_self"
                , A.href href
                , A.title tooltip
                , A.classList [ ( "active", active ) ]
                , onClickLink (SetActiveFork p.topicId p.seq fork)
                ]
                [ H.text text
                ]

        postForkSelector p =
            let
                verifiedForks =
                    Discourse.verifiedForks p
            in
            if not (List.isEmpty verifiedForks) then
                let
                    forkHelp =
                        "Click on a number to select a fork."

                    linkList =
                        verifiedForks
                            |> List.indexedMap (\i fromTopicId -> forkLink p (Just fromTopicId) (String.fromInt (i + 1)))
                            |> (::) (forkLink p Nothing "0")
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
                [ H.text p.username ]

        viewPost p1 p =
            let
                showAuthor =
                    Set.member ( p.topicId, p.seq ) r.showAuthor

                idAttr =
                    if topicId == p1.topicId && p1.seq == p.seq then
                        []

                    else
                        [ A.id (domId p.topicId p.seq) ]
            in
            [ H.article
                (List.append
                    idAttr
                    [ A.class "post"
                    , E.onClick (r.toMsg (SetShowAuthor p.topicId p.seq (not showAuthor)))
                    ]
                )
                (viewAuthor p showAuthor :: HtmlUtil.toVirtualDom p.body)
            , postForkSelector p
            ]

        controls =
            H.div
                [ A.class "controls" ]
                [ H.span [ A.class "icon-yinyang", E.onClick (r.toMsg ToggleYinYang) ] []
                ]
    in
    case ( topic, post1 ) of
        ( Just t, Just p1 ) ->
            let
                headerId =
                    if topicId == p1.topicId then
                        -- Is first post at the beginning of the read topic or a parent?
                        domId p1.topicId p1.seq

                    else
                        "thread-start"
            in
            [ controls
            , H.section
                []
                (H.h1 [ A.id headerId ] [ H.text t.title ]
                    :: List.concatMap (viewPost p1) (thread Set.empty p1)
                )
            , H.footer [] []
            ]

        _ ->
            [ controls ]

module Discourse exposing (Fork, Post, Topic, TopicId(..), TopicResult, fetchTopic, firstUnavailableParentTopicId, firstUnverifiedForkTopicId, forkTopicId, getPost, getRoot, isPotentialFork, isVerifiedFork, mapFirstPost, nextPost, parentTopicAndPostId, setActiveFork, topicAndPostIdFromUrl, topicIdFromSlug, updatePost, verifiedForks, verifyForks)

import Dict exposing (Dict)
import Html.Parser
import Http
import Json.Decode as D
import Parser as P exposing ((|.), (|=))
import Result exposing (Result)
import Set
import Url exposing (Url)
import Url.Builder as B


type TopicId
    = Id Int
    | Slug String


type Fork
    = Potential Int -- topic ids
    | Verified Int


type alias Post =
    { topicId : Int
    , seq : Int
    , name : Maybe String
    , username : String
    , body : List Html.Parser.Node
    , activeFork : Maybe Int
    , forks : List Fork
    , parent : Maybe ( Int, Int )
    }


type alias Topic =
    { title : String
    , slug : String
    , posts : Dict Int Post -- key is post sequence number
    }


type alias TopicResult =
    Result Http.Error ( Int, Topic )


isVerifiedFork : Fork -> Bool
isVerifiedFork f =
    case f of
        Verified _ ->
            True

        _ ->
            False


isPotentialFork : Fork -> Bool
isPotentialFork f =
    not (isVerifiedFork f)


forkTopicId : Fork -> Int
forkTopicId f =
    case f of
        Potential p ->
            p

        Verified v ->
            v


verifiedForks : Post -> List Int
verifiedForks p =
    p.forks
        |> List.filter isVerifiedFork
        |> List.map forkTopicId


setActiveFork : Maybe Int -> Post -> Post
setActiveFork fork post =
    { post | activeFork = fork }


verifyForks : Dict Int Topic -> Post -> Post
verifyForks ts p =
    let
        verifyFork f =
            case f of
                Potential fid ->
                    Dict.get fid ts
                        |> Maybe.andThen (getPost 1)
                        |> Maybe.andThen .parent
                        |> Maybe.map
                            (\parent ->
                                if ( p.topicId, p.seq ) == parent then
                                    Verified fid

                                else
                                    Potential fid
                            )
                        |> Maybe.withDefault f

                _ ->
                    f
    in
    { p | forks = List.map verifyFork p.forks }


mapFirst : (a -> Maybe b) -> List a -> Maybe b
mapFirst pred list =
    case list of
        x :: xs ->
            case pred x of
                (Just _) as res ->
                    res

                _ ->
                    mapFirst pred xs

        _ ->
            Nothing


mapFirstPost : (Post -> Maybe b) -> Topic -> Maybe b
mapFirstPost pred topic =
    mapFirst pred (Dict.values topic.posts)


firstUnavailableParentTopicId : Dict Int Topic -> Int -> Maybe Int
firstUnavailableParentTopicId topics topicId =
    let
        f visited t =
            let
                maybeTopic =
                    Dict.get t topics

                maybeParent =
                    Maybe.andThen (getPost 1) maybeTopic
                        |> Maybe.andThen .parent
                        |> Maybe.map Tuple.first
            in
            case ( Set.member t visited, maybeTopic, maybeParent ) of
                ( True, _, _ ) ->
                    Nothing

                ( False, Nothing, _ ) ->
                    Just t

                ( False, Just _, Nothing ) ->
                    Nothing

                ( False, Just _, Just p ) ->
                    f (Set.insert t visited) p
    in
    f Set.empty topicId


firstUnverifiedForkTopicId : Dict Int Topic -> Maybe ( Post, Int )
firstUnverifiedForkTopicId topics =
    let
        verifiedFork : Post -> Fork -> Maybe ( Post, Int )
        verifiedFork p f =
            case f of
                Verified id ->
                    Just ( p, id )

                _ ->
                    Nothing
    in
    Dict.toList topics
        |> mapFirst
            (\( _, topic ) ->
                topic.posts
                    |> Dict.toList
                    |> mapFirst (\( _, p ) -> mapFirst (verifiedFork p) p.forks)
            )


topicIdFromSlug : Dict Int Topic -> TopicId -> Maybe Int
topicIdFromSlug topics id =
    case id of
        Slug slug ->
            topics
                |> Dict.toList
                |> mapFirst
                    (\( k, v ) ->
                        if v.slug == slug then
                            Just k

                        else
                            Nothing
                    )

        Id i ->
            Just i


topicAndPostIdFromUrl : Url -> String -> Maybe ( Int, Int )
topicAndPostIdFromUrl srvUrl url =
    let
        urlStr =
            Url.toString srvUrl

        postNrOrDefault def =
            P.oneOf
                [ P.succeed identity |. P.token "/" |= P.int
                , P.succeed def
                ]

        topicAndPost =
            P.succeed Tuple.pair
                |. P.token urlStr
                |. P.token "t/"
                |. P.chompUntil "/"
                |. P.token "/"
                |= P.int
                |= postNrOrDefault 1
    in
    P.run topicAndPost url
        |> Result.toMaybe


maybeOr : Maybe a -> Maybe a -> Maybe a
maybeOr a b =
    case ( a, b ) of
        ( Just _, _ ) ->
            a

        ( Nothing, _ ) ->
            b


extractAndFilterForkLink : Url -> List Html.Parser.Node -> ( Maybe ( Int, Int ), List Html.Parser.Node )
extractAndFilterForkLink srvUrl nodes =
    let
        f ns =
            case ns of
                h :: t ->
                    case h of
                        Html.Parser.Element "a" [ ( "href", url ) ] [ Html.Parser.Text "!FORK" ] ->
                            let
                                ( _, filtered ) =
                                    f t
                            in
                            ( topicAndPostIdFromUrl srvUrl url, filtered )

                        Html.Parser.Element tag attrs children ->
                            let
                                ( url0, filteredChildren ) =
                                    f children

                                ( url1, filteredTail ) =
                                    f t
                            in
                            ( maybeOr url0 url1, Html.Parser.Element tag attrs filteredChildren :: filteredTail )

                        _ ->
                            let
                                ( url, filtered ) =
                                    f t
                            in
                            ( url, h :: filtered )

                [] ->
                    ( Nothing, [] )
    in
    f nodes


decodePost : Url -> Maybe ( Int, Int ) -> D.Decoder Post
decodePost srvUrl forkInfo =
    let
        htmlString html =
            case Html.Parser.run html of
                Ok nodes ->
                    D.succeed nodes

                Err e ->
                    D.fail (P.deadEndsToString e)

        setActiveForkFromLoadTrail postNr =
            case forkInfo of
                Just ( postIdToFork, forksToTopicId ) ->
                    if postNr == postIdToFork then
                        Just forksToTopicId

                    else
                        Nothing

                _ ->
                    Nothing

        forkUrl s =
            case topicAndPostIdFromUrl srvUrl s of
                Just ids ->
                    D.succeed ids

                _ ->
                    D.fail "Invalid link_counts[] url."
    in
    D.field "cooked" D.string
        |> D.andThen htmlString
        |> D.map (extractAndFilterForkLink srvUrl)
        |> D.andThen
            (\( parent, html ) ->
                D.map8 Post
                    (D.field "topic_id" D.int)
                    (D.field "post_number" D.int)
                    (D.field "name" (D.maybe D.string))
                    (D.field "username" D.string)
                    (D.succeed html)
                    (D.field "post_number" D.int
                        |> D.map setActiveForkFromLoadTrail
                    )
                    (D.oneOf
                        -- TODO: Use pipeline parser with optional here instead
                        [ (D.field "link_counts" <| D.list <| D.field "url" <| D.andThen forkUrl <| D.string)
                            |> D.map
                                (List.filter (\( _, pnr ) -> pnr == 1)
                                    >> List.map (Tuple.first >> Potential)
                                )
                        , D.succeed []
                        ]
                    )
                    (D.succeed parent)
            )


decodeTopic : Url -> Maybe ( Int, Int ) -> D.Decoder Topic
decodeTopic srvUrl forkInfo =
    D.map3 Topic
        (D.field "title" D.string)
        (D.field "slug" D.string)
        (D.at [ "post_stream", "posts" ]
            (D.map2 Tuple.pair
                (D.field "post_number" D.int)
                (decodePost srvUrl forkInfo)
                |> D.list
            )
            |> D.map Dict.fromList
        )


nextPost : Post -> Dict Int Topic -> Maybe Post
nextPost p topics =
    case p.activeFork of
        Just tid ->
            Dict.get tid topics
                |> Maybe.andThen (getPost 1)

        _ ->
            Dict.get p.topicId topics
                |> Maybe.andThen (getPost (p.seq + 1))


getPost : Int -> Topic -> Maybe Post
getPost n topic =
    Dict.get n topic.posts


getRoot : Dict Int Topic -> Int -> Maybe Topic
getRoot topics topicId =
    let
        f visited id =
            let
                t =
                    Dict.get id topics

                parent =
                    t
                        |> Maybe.andThen (getPost 1)
                        |> Maybe.andThen .parent
            in
            case ( Set.member topicId visited, parent ) of
                ( False, Just ( parentTopicId, _ ) ) ->
                    f (Set.insert topicId visited) parentTopicId

                _ ->
                    t
    in
    f Set.empty topicId


updatePost : Int -> Int -> (Post -> Post) -> Dict Int Topic -> Dict Int Topic
updatePost topicId postNr f topics =
    Dict.update topicId (Maybe.map (\t -> { t | posts = Dict.update postNr (Maybe.map f) t.posts })) topics


parentTopicAndPostId : Int -> Dict Int Topic -> Maybe ( Int, Int )
parentTopicAndPostId tid ts =
    Dict.get tid ts
        |> Maybe.andThen (getPost 1)
        |> Maybe.andThen .parent


fetchTopic : Url -> TopicId -> Maybe ( Int, Int ) -> (TopicResult -> msg) -> Cmd msg
fetchTopic srvUrl tid forkInfo toMsg =
    let
        idOrSlug =
            case tid of
                Slug s ->
                    s

                Id i ->
                    String.fromInt i

        srvUrlStr =
            let
                s =
                    Url.toString srvUrl
            in
            if String.endsWith "/" s then
                String.dropRight 1 s

            else
                s

        url =
            B.crossOrigin srvUrlStr [ "t", idOrSlug ++ ".json" ] []
    in
    Http.get
        { url = url
        , expect = Http.expectJson toMsg (D.map2 Tuple.pair (D.field "id" D.int) (decodeTopic srvUrl forkInfo))
        }

module Discourse exposing (Post, Topic, TopicId(..), TopicResult, fetchTopic, getPost, nextPost, parentTopicAndPostId, topicAndPostIdFromUrl)

import Dict exposing (Dict)
import Html.Parser
import Http
import Json.Decode as D
import Parser as P exposing ((|.), (|=))
import Result exposing (Result)
import Url exposing (Url)
import Url.Builder as B


type TopicId
    = Id Int
    | Slug String


type alias Post =
    { topicId : Int
    , seq : Int
    , name : Maybe String
    , username : String
    , body : List Html.Parser.Node
    , activeFork : Maybe ( Int, Int )
    , forks : List ( Int, Int )
    , parent : Maybe ( Int, Int )
    }


type alias Topic =
    { title : String
    , posts : Dict Int Post -- key is post sequence number
    }


topicAndPostIdFromUrl : Url -> String -> Maybe ( Int, Int )
topicAndPostIdFromUrl srvUrl url =
    let
        urlStr =
            Url.toString srvUrl

        topicAndPost =
            P.succeed Tuple.pair
                |. P.symbol urlStr
                |. P.symbol "t/"
                |. P.chompUntil "/"
                |. P.symbol "/"
                |= P.int
                |. P.symbol "/"
                |= P.int
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


decodePost : Url -> Int -> Maybe ( Int, Int ) -> D.Decoder Post
decodePost srvUrl topicId forkInfo =
    let
        htmlString html =
            case Html.Parser.run html of
                Ok nodes ->
                    D.succeed nodes

                Err e ->
                    D.fail (P.deadEndsToString e)

        setActiveFork postNr =
            case forkInfo of
                Just ( postIdToFork, forksToTopicId ) ->
                    if postNr == postIdToFork then
                        Just ( forksToTopicId, 1 )

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
                    (D.succeed topicId)
                    (D.field "post_number" D.int)
                    (D.field "name" (D.maybe D.string))
                    (D.field "username" D.string)
                    (D.succeed html)
                    (D.field "post_number" D.int
                        |> D.map setActiveFork
                    )
                    (D.oneOf
                        -- TODO: Use pipeline parser with optional here instead
                        [ D.field "link_counts" <| D.list <| D.field "url" <| D.andThen forkUrl <| D.string
                        , D.succeed []
                        ]
                    )
                    (D.succeed parent)
            )


decodeTopic : Url -> Maybe ( Int, Int ) -> D.Decoder Topic
decodeTopic srvUrl forkInfo =
    D.map2 Topic
        (D.field "title" D.string)
        (D.field "id" D.int
            |> D.andThen
                (\tid ->
                    D.at [ "post_stream", "posts" ]
                        (D.map2 Tuple.pair
                            (D.field "post_number" D.int)
                            (decodePost srvUrl tid forkInfo)
                            |> D.list
                        )
                        |> D.map Dict.fromList
                )
        )


type alias TopicResult =
    Result Http.Error ( Int, Topic )


nextPost : Post -> Dict Int Topic -> Maybe Post
nextPost p topics =
    case p.activeFork of
        Just ( tid, pnr ) ->
            Dict.get tid topics
                |> Maybe.andThen (getPost pnr)

        _ ->
            Dict.get p.topicId topics
                |> Maybe.andThen (getPost (p.seq + 1))


getPost : Int -> Topic -> Maybe Post
getPost n topic =
    Dict.get n topic.posts


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



-- fetchPost

module Discourse exposing (..)

import Dict exposing (Dict)
import Http
import Json.Decode as D
import Result exposing (Result)
import Url exposing (Url)
import Url.Builder as B


type TopicId
    = Id Int
    | Slug String


type alias Post =
    { name : Maybe String
    , username : String
    , body : String

    --, parent : Maybe Id
    }


type alias Topic =
    { posts : Dict Int Post
    , stream : List Int
    }



--decodePostParent : D.Decoder Maybe Int
--decodePostParent


decodePost : D.Decoder Post
decodePost =
    D.map3 Post
        (D.field "name" (D.maybe D.string))
        (D.field "username" D.string)
        (D.field "cooked" D.string)


decodeTopic : D.Decoder Topic
decodeTopic =
    D.map2 Topic
        (D.at [ "post_stream", "posts" ]
            (D.map2 Tuple.pair
                (D.field "id" D.int)
                decodePost
            )
            |> D.list
            |> D.map Dict.fromList
        )
        (D.at [ "post_stream", "stream" ] (D.list D.int))


type alias TopicResult =
    Result Http.Error ( Int, Topic )


fetchTopic : Url -> TopicId -> (TopicResult -> msg) -> Cmd msg
fetchTopic server tid toMsg =
    let
        idOrSlug =
            case tid of
                Slug s ->
                    s

                Id i ->
                    String.fromInt i

        url =
            B.crossOrigin (Url.toString server) [ "t", idOrSlug ] []
    in
    Http.get
        { url = url
        , expect = Http.expectJson toMsg (D.map2 Tuple.pair (D.field "id" D.int) decodeTopic)
        }



-- fetchPost

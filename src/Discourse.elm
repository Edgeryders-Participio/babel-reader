module Discourse exposing (..)

import Url exposing (Url)
import Dict exposing (Dict)
import Http
import Result exposing (Result)
import Json.Decode as D

type alias Id = Int

type alias Post =
    { name : Maybe String
    , username : String
    , body : String
    }

type alias Topic =
    { posts : Dict Id Post
    , stream : List Id
    }

decodePost : D.Decoder Post
decodePost =
    D.map3 Post
        (D.field "name" (D.maybe D.string))
        (D.field "username" D.string)
        (D.field "cooked" D.string)

decodeTopic : D.Decoder Topic
decodeTopic = 
    D.map2 Topic
        (D.at ["post_stream", "posts"]
            (D.map2 Tuple.pair
                (D.field "id" D.int)
                decodePost)
            |> D.list
            |> D.map Dict.fromList
        )
        (D.at ["post_stream", "stream"] (D.list D.int))


type alias TopicResult = Result Http.Error Topic

fetchTopic : Url -> Id -> (TopicResult -> msg) -> Cmd msg 
fetchTopic server id toMsg =
    let
        url = server
    in
        Http.get
            { url = Url.toString url
            , expect = Http.expectJson toMsg decodeTopic
            }

-- fetchPost
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
import Reader
import Set exposing (Set)
import Task
import Url
import Url.Builder as B


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


type PageState
    = Error String
    | LoadingThread Discourse.TopicId (Reader.Model Msg) (Maybe ( Int, Int ))
    | Reader Int (Reader.Model Msg)


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
                ( Ok srvUrl, ReadTopic topicId showPost ) ->
                    let
                        ( reader, readerCmd ) =
                            Reader.init srvUrl ReaderMsg topicId
                    in
                    ( LoadingThread topicId reader showPost
                    , readerCmd
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
        , Reader.applyTheme Reader.Yin
        ]
    )


type Msg
    = LinkClicked Browser.UrlRequest
    | UrlChanged Url.Url
    | ReaderMsg Reader.Msg


switchToTopic : Int -> Maybe ( Int, Int ) -> Model -> ( Model, Cmd Msg )
switchToTopic topicId scrollTo model =
    ( model, Cmd.none )



-- TODO


update : Msg -> Model -> ( Model, Cmd Msg )
update msg model =
    case msg of
        LinkClicked urlRequest ->
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
                                        [ Nav.pushUrl model.key (Url.toString url)
                                        ]
                                    )
                                |> Maybe.withDefault []
                                |> Cmd.batch
                            )

                Browser.External href ->
                    case model.state of
                        _ ->
                            ( model, Nav.load href )

        UrlChanged url ->
            ( { model | url = url }
            , Cmd.none
            )

        ReaderMsg readerMsg ->
            case model.state of
                Reader topicId reader ->
                    let
                        ( newReader, cmd ) =
                            Reader.update readerMsg reader
                    in
                    ( { model | state = Reader topicId newReader }, cmd )

                LoadingThread topicIdOrSlug reader scrollTo ->
                    let
                        ( newReader, cmd ) =
                            Reader.update readerMsg reader

                        finishedTopicId =
                            Reader.hasFinishedLoading newReader topicIdOrSlug

                        newReaderWithSelectedForks =
                            finishedTopicId
                                |> Maybe.map (Reader.selectForksForTopic newReader)
                                |> Maybe.withDefault newReader
                    in
                    case finishedTopicId of
                        Just topicId ->
                            let
                                scrollCmd =
                                    case scrollTo of
                                        Just ( scrollToTopicId, scrollToPostNr ) ->
                                            Reader.scrollToPost newReaderWithSelectedForks scrollToTopicId scrollToPostNr

                                        Nothing ->
                                            Reader.scrollToPost newReaderWithSelectedForks topicId 1
                            in
                            ( { model | state = Reader topicId newReaderWithSelectedForks }
                            , Cmd.batch
                                [ cmd
                                , scrollCmd
                                ]
                            )

                        Nothing ->
                            ( { model | state = LoadingThread topicIdOrSlug newReader scrollTo }, cmd )

                Error _ ->
                    ( model, Cmd.none )


subscriptions : Model -> Sub Msg
subscriptions model =
    Sub.none


view : Model -> Browser.Document Msg
view model =
    let
        title =
            Maybe.withDefault "Babel Between Us Reader" <|
                case model.state of
                    Reader topicId readerModel ->
                        Reader.topicTitle readerModel topicId

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

            Reader topicId readerModel ->
                Reader.view readerModel topicId
    }

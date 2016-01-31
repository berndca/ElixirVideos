module Videos.WebApi (Videos, ApiData, Action, Date2, getVideos, initialModel, update) where

import Http
import String
import Task exposing (Task)
import Effects exposing (Effects)
import Json.Decode as Decode exposing (Decoder, andThen, object2, map, string, list, int, (:=))


videosUrl : String
videosUrl =
    "elixir.json"


type Action
    = Receive (Result Http.Error ApiData)
    | RequestMore


type alias Videos =
    List VideoRecord


type alias ApiData =
    { created : Date2
    , videos : Videos
    }


type alias VideoRecord =
    { id : String
    , title : String
    , pub : Date2
    , total : Int
    }


type alias Date2 =
    { ds : Int
    , repr : String
    }


initialModel : ApiData
initialModel =
    { created = { repr = "15 Jan 2016", ds = 749967 }
    , videos = []
    }


andMap =
    object2 (<|)


modelDecoder : Decoder ApiData
modelDecoder =
    ApiData
        `map` ("created" := dateDecoder)
        `andMap` ("videos" := (list videoDecoder))


videoDecoder : Decoder VideoRecord
videoDecoder =
    VideoRecord
        `map` ("id" := string)
        `andMap` ("title" := string)
        `andMap` ("pub" := dateDecoder)
        `andMap` ("total" := int)


dateDecoder : Decoder Date2
dateDecoder =
    Date2
        `map` ("ds" := int)
        `andMap` ("repr" := string)


getVideos : Effects Action
getVideos =
    Http.get modelDecoder videosUrl
        |> Task.toResult
        |> Task.map Receive
        |> Effects.task


update : Action -> ApiData -> ( ApiData, Effects Action )
update action model =
    case action of
        RequestMore ->
            ( model, getVideos )

        Receive result ->
            case result of
                Result.Ok model ->
                    ( model, Effects.none )

                Result.Err err ->
                    let
                        _ = Debug.log "Receive Error" err
                    in
                        ( initialModel, Effects.none )

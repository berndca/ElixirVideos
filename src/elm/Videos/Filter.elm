module Videos.Filter (Action(..), Model, filterVideos, initialState, update, view) where

import String
import Maybe exposing (withDefault)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Videos.WebApi exposing (Date2, Videos)


type Action
    = FilterTitles String
    | SetTimeFrame String


type alias Model =
    { timeFrame : String
    , filterInput : String
    }


initialState : Model
initialState =
    { timeFrame = "Any time"
    , filterInput = ""
    }


timeFrames : List String
timeFrames =
    [ "Any time"
    , "Past quarter"
    , "Past year"
    ]



--UPDATE


update : Action -> Model -> Model
update action model =
    case action of
        SetTimeFrame str ->
            { model | timeFrame = str }

        FilterTitles str ->
            { model | filterInput = str }


filterCriteria filterStr video =
    let
        videoTitleLower = String.toLower video.title

        filterItems = String.split " " filterStr |> List.filter (String.isEmpty >> not)
    in
        List.all (\str -> (String.contains str videoTitleLower)) filterItems


filterSearch : String -> Videos -> Videos
filterSearch filterStr videos =
    let
        filterLower = String.toLower filterStr
    in
        List.filter (filterCriteria filterLower) videos


filterByDate : String -> Date2 -> Videos -> Videos
filterByDate option created videos =
    case option of
        "Past year" ->
            List.filter (\v -> v.pub.ds > (created.ds - 372)) videos

        "Past quarter" ->
            List.filter (\v -> v.pub.ds > (created.ds - 93)) videos

        _ ->
            videos


filterVideos : Model -> Date2 -> Videos -> Videos
filterVideos model created videos =
    videos
        |> filterSearch model.filterInput
        |> filterByDate model.timeFrame created



-- VIEW


filterInput : Signal.Address Action -> String -> Html
filterInput address filterValue =
    div
        [ class "ui fluid icon input" ]
        [ i [ class "search icon" ] []
        , input
            [ placeholder "Search"
            , type' "text"
            , value filterValue
            , on "input" targetValue (Signal.message address << FilterTitles)
            ]
            []
        ]


timeOption : Signal.Address Action -> String -> String -> Html
timeOption address timeValue option =
    div
        [ classList [ ( "active", timeValue == option ), ( "item", True ) ]
        , onClick address (SetTimeFrame option)
        ]
        [ text option ]


timeOptions : Signal.Address Action -> String -> Html
timeOptions address timeValue =
    div
        [ class "ui right compact menu" ]
        [ div
            [ class "ui simple dropdown item" ]
            [ span [] [ text timeValue ]
            , i [ class "dropdown icon" ] []
            , div
                [ class "menu" ]
                (List.map (timeOption address timeValue) timeFrames)
            ]
        ]


view : Signal.Address Action -> Model -> Html
view address model =
    div
        [ class "ui two column unstackable grid" ]
        [ div
            [ class "column" ]
            [ filterInput address model.filterInput
            ]
        , div
            [ class "right aligned column" ]
            [ timeOptions address model.timeFrame
            ]
        ]

module Videos.Table (Action(..), init, update, view) where

import Maybe exposing (withDefault)
import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String
import Components.Table as Table
import Components.Link as Link
import Videos.WebApi exposing (Videos)


type Action
    = TableAction Table.Action


headerOptions : List Table.ColumnHeader
headerOptions =
    [ { title = "Title", iconType = Just "alphabet", defaultAsc = Just True }
    , { title = "Views", iconType = Just "numeric", defaultAsc = Just False }
    , { title = "Published", iconType = Just "content", defaultAsc = Just False }
    ]


init : Table.Model
init =
    { headerOptions = headerOptions
    , sortColumn = "Views"
    , sortAscending = False
    , currentPage = 1
    , itemsPerPage = 15
    }


link row =
    Link.view
        { href = "https://youtu.be/" ++ (String.dropLeft 1 row.id)
        , isIcon = False
        , text = row.title
        }



-- UPDATE


update : Action -> Table.Model -> Table.Model
update action header =
    case action of
        TableAction acc ->
            Table.update acc header



-- VIEW


sortVideos : String -> Videos -> Table.Body
sortVideos sortColumn sVideos =
    let
        sorted =
            case sortColumn of
                "Title" ->
                    List.sortBy .title sVideos

                "Published" ->
                    List.sortBy (\v -> v.pub.ds) sVideos

                _ ->
                    List.sortBy .total sVideos
    in
        List.map (\v -> [ link v, text (toString v.total), text v.pub.repr ]) sorted


videosToTable : Table.Model -> Videos -> Table.Body
videosToTable header videos =
    let
        sortedVideos =
            videos
                |> sortVideos header.sortColumn
    in
        if header.sortAscending then
            sortedVideos
        else
            List.reverse sortedVideos


view : Signal.Address Action -> Table.Model -> Videos -> Html
view address header videos =
    let
        filteredVideos = videosToTable header videos
    in
        Table.view
            (Signal.forwardTo address TableAction)
            { model = header
            , body = filteredVideos
            }

module Videos.Main (Action(..), Model, init, initialModel, update, view) where

import Effects exposing (Effects)
import Html exposing (Html, div, text)
import Html.Attributes exposing (class)

import Components.Table
import Videos.Filter as Filter
import Videos.WebApi as WebApi
import Videos.Table as Table


type Action
    = TableAction Table.Action
    | WebApiAction WebApi.Action
    | FilterAction Filter.Action


type alias Model =
    { apiData : WebApi.ApiData
    , header : Components.Table.Model
    , filterState : Filter.Model
    }


initialModel : Model
initialModel =
    { apiData = WebApi.initialModel
    , header = Table.init
    , filterState = Filter.initialState
    }


init : ( Model, Effects Action )
init =
    ( initialModel
    , Effects.map WebApiAction WebApi.getVideos
    )



-- UPDATE


update : Action -> Model -> ( Model, Effects Action )
update action model =
    case action of
        WebApiAction act ->
            let
                ( apiData, fx ) = WebApi.update act model.apiData
            in
                ( { model | apiData = apiData }
                , Effects.map WebApiAction fx
                )

        FilterAction act ->
            ( { model | filterState = Filter.update act model.filterState }
            , Effects.none
            )

        TableAction act ->
            ( { model | header = Table.update act model.header }
            , Effects.none
            )


view : Signal.Address Action -> Model -> Html
view address model =
    let
        filteredVideos = Filter.filterVideos model.filterState model.apiData.created model.apiData.videos
    in
        div
            [ class "main ui container" ]
            [ div [ class "ui large header" ] [ text "Elixir Videos" ]
            , Filter.view (Signal.forwardTo address FilterAction) model.filterState
            , Table.view (Signal.forwardTo address TableAction) model.header filteredVideos
            ]

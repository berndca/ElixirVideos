module Components.Table (Model, Body, ColumnHeader, init, Action(..), update, view) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import Maybe exposing (withDefault)
import String
import Components.Pagination as Pagination


-- MODEL


type alias Row =
    List Html


type alias Body =
    List Row


type alias ColumnHeader =
    { title : String
    , iconType : Maybe String
    , defaultAsc : Maybe Bool
    }


type alias Model =
    { headerOptions : List ColumnHeader
    , sortColumn : String
    , sortAscending : Bool
    , currentPage : Int
    , itemsPerPage : Int
    }


type alias TableModel =
    { model : Model, body : Body }


init : Model -> Body -> TableModel
init model body =
    { model = model, body = body }



-- UPDATE


type Action
    = PageAction Pagination.Action
    | ToggleSort ColumnHeader


isSortable : ColumnHeader -> List ColumnHeader -> Bool
isSortable current cols =
    List.any (\col -> (col.title == current.title) && (col.iconType /= Nothing)) cols


update : Action -> Model -> Model
update action model =
    case action of
        PageAction acc ->
            case acc of
                Pagination.SetPage pageNo ->
                    { model | currentPage = pageNo }

        ToggleSort col ->
            if isSortable col model.headerOptions then
                if model.sortColumn == col.title then
                    { model | sortAscending = not model.sortAscending, currentPage = 1 }
                else
                    { model
                        | sortAscending = withDefault False col.defaultAsc
                        , sortColumn = col.title
                        , currentPage = 1
                    }
            else
                model



-- VIEW


getPage : TableModel -> Body
getPage model =
    model.body
        |> List.drop ((model.model.currentPage - 1) * model.model.itemsPerPage)
        |> List.take model.model.itemsPerPage


toIcon : Maybe String -> Bool -> Html
toIcon option asc =
    case option of
        Just iconType ->
            i
                [ class
                    ("sort "
                        ++ iconType
                        ++ " "
                        ++ (if asc then
                                "a"
                            else
                                "de"
                           )
                        ++ "scending icon"
                    )
                ]
                []

        _ ->
            p [] [ text "" ]


tH : Signal.Address Action -> String -> Bool -> ColumnHeader -> Html
tH address sortColumn sortAsc colHeader =
    let
        isSortColumn = sortColumn == colHeader.title

        icon = toIcon colHeader.iconType sortAsc
    in
        if isSortColumn then
            th [ onClick address (ToggleSort colHeader) ] ([ text (colHeader.title ++ " ") ] ++ [ icon ])
        else
            th [ onClick address (ToggleSort colHeader) ] [ text colHeader.title ]


tHeader : Signal.Address Action -> Model -> Html
tHeader address model =
    thead
        []
        [ tr [] (List.map (tH address model.sortColumn model.sortAscending) model.headerOptions)
        ]


tFooter : Signal.Address Action -> TableModel -> Html
tFooter address model =
    tfoot
        []
        [ tr
            []
            [ th
                [ colspan (List.length model.model.headerOptions) ]
                (Pagination.listView
                    (Signal.forwardTo address PageAction)
                    { itemsPerPage = model.model.itemsPerPage
                    , totalItems = (List.length model.body)
                    }
                    model.model.currentPage
                )
            ]
        ]


row : List Html -> Html
row elements =
    tr [] (List.map (\el -> td [] [ el ]) elements)


view : Signal.Address Action -> TableModel -> Html
view address tableModel =
    table
        [ class "ui unstackable celled table" ]
        [ tHeader address tableModel.model
        , tbody [] (List.map row (getPage tableModel))
        , tFooter address tableModel
        ]

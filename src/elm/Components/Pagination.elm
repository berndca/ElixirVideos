module Components.Pagination (Action(..), listView) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)
import String


type alias Props =
    { itemsPerPage : Int
    , totalItems : Int
    }


type alias Range =
    { start : Int
    , stop : Int
    }


type Action
    = SetPage Int


pages : Props -> Int
pages props =
    (ceiling (toFloat (props.totalItems) / toFloat (props.itemsPerPage)))


middleRange : Props -> Int -> Range
middleRange props currentPage =
    let
        minPage = Basics.max 2 (currentPage - 1)

        maxPage = Basics.min ((pages props) - 1) (minPage + 2)
    in
        { start = Basics.max 2 (maxPage - 2), stop = maxPage }


optionalBridge : Bool -> List Html
optionalBridge exists =
    if exists then
        [ a [ class "item" ] [ text "..." ] ]
    else
        []


singleLink : Signal.Address Action -> Int -> Int -> Html
singleLink address currentPage forPage =
    a
        [ classList [ ( "active", forPage == currentPage ), ( "item", True ) ]
        , onClick address (SetPage forPage)
        ]
        [ text (toString forPage) ]


status : Props -> Int -> Html
status props currentPage =
    let
        firstItem = (currentPage - 1) * props.itemsPerPage

        lastItem = Basics.min props.totalItems (firstItem + props.itemsPerPage)

        itemsText =
            String.join
                " "
                [ "Items"
                , (toString firstItem)
                , "-"
                , (toString lastItem)
                , "out of"
                , (toString props.totalItems)
                ]
    in
        div
            [ class "ui floated pagination menu width-640-plus", id "optional" ]
            [ div [ class "item" ] [ text itemsText ]
            ]


body : Signal.Address Action -> Props -> Int -> List Html
body address props currentPage =
    let
        range = middleRange props currentPage

        totalPages = pages props

        next = clamp 1 totalPages (currentPage + 1)

        previous = clamp 1 totalPages (currentPage - 1)
    in
        if (pages props) == 1 then
            [ singleLink address 1 1 ]
        else
            [ a [ class "icon item", onClick address (SetPage previous) ] [ i [ class "left chevron icon" ] [] ] ]
                ++ [ singleLink address currentPage 1 ]
                ++ optionalBridge (range.start > 2)
                ++ List.map (singleLink address currentPage) [range.start..range.stop]
                ++ optionalBridge (range.stop < (totalPages - 1))
                ++ [ singleLink address currentPage totalPages ]
                ++ [ a [ class "icon item", onClick address (SetPage next) ] [ i [ class "right chevron icon" ] [] ] ]


listView : Signal.Address Action -> Props -> Int -> List Html
listView address props currentPage =
    [ (status props currentPage)
    , div [ class "ui right floated pagination menu" ] (body address props currentPage)
    ]

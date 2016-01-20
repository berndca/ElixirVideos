module Components.Link (view) where

import Html exposing (..)
import Html.Attributes exposing (..)
import Html.Events exposing (..)


type alias Props =
    { href : String
    , isIcon : Bool
    , text : String
    }


view : Props -> Html
view props =
    let
        node =
            if props.isIcon then
                [ i [ class props.text ] [] ]
            else
                [ text props.text ]
    in
        a [ href props.href, target "_blank" ] node

module ConstantForStyle exposing (..)

import Html exposing (Html, div)
import Html.Attributes exposing (style)


element : Html msg
element =
    div
        [ style "padding" "0"
        , style "margin" "0"
        ]
        []

module ForbiddenFeatures exposing (..)


applyForbiddenOperator : Int -> String
applyForbiddenOperator i =
    i |> String.fromInt


mapList : List String -> List String
mapList list =
    List.map (\e -> "value: " ++ e) list

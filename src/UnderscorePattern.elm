module UnderscorePattern exposing (..)


type CustomType
    = First
    | Second
    | Result
    | Next


foo : CustomType -> String
foo t =
    case t of
        First ->
            "First"

        _ ->
            "Rest"


type Key
    = Up
    | Down


toKey : String -> Key
toKey string =
    case string of
        "Up" ->
            Up

        _ ->
            Down

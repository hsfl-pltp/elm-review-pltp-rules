module Negation exposing (..)

foo : Int -> Int -> Bool
foo a b = 
    not (a == b || b >= 7)
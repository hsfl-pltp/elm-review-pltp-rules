module EtaReduction exposing (..)

incList : List Int -> List Int
incList list = 
    List.map inc list

inc : Int -> Int 
inc i =
    1 + i
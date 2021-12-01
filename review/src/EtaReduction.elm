module EtaReduction exposing (..)

incList : List Int -> List Int
incList  = 
    List.map inc 

inc : Int -> Int 
inc i =
    1 + i
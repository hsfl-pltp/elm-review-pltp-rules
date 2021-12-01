module CommutingConversion exposing (..)


type MyType 
    = One
    | Two
    | Three

f : Int -> Int
f i =
    i + i


calc : Bool -> Int -> Int -> Int
calc a b c =
    if a then
        f b

    else
        f c

calc2 : MyType -> Int -> Int -> Int -> Int
calc2 t a b c=
    case t of
        One -> 
            f a
        Two ->
            f b
        Three ->
            f c
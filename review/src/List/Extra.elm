module List.Extra exposing (combinations, find, last, some, sumBy, unique)


find : (a -> Bool) -> List a -> Maybe a
find pred l =
    case l of
        [] ->
            Nothing

        x :: xs ->
            if pred x then
                Just x

            else
                find pred xs


last : List a -> Maybe a
last list =
    case list of
        [] ->
            Nothing

        x :: [] ->
            Just x

        _ :: xs ->
            last xs


some : (a -> Bool) -> List a -> Bool
some pred list =
    let
        p e acc =
            if pred e then
                acc + 1

            else
                acc
    in
    List.foldl p 0 list >= 2


combinations : List a -> List b -> List ( a, b )
combinations listA listB =
    case listA of
        [] ->
            []

        x :: xs ->
            List.map (\y -> ( x, y )) listB ++ combinations xs listB


sumBy : (a -> Int) -> List a -> Int
sumBy pred =
    List.foldl (\e acc -> pred e + acc) 0


unique : List a -> List a
unique list =
    case list of
        [] ->
            []

        x :: xs ->
            x :: unique (List.filter ((/=) x) xs)

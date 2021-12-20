module List.Extra exposing (find, last, some)


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
    
    (List.foldl p 0 list) >= 2

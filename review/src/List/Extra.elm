module List.Extra exposing (find, last)


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

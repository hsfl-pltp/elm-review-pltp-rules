module List.Extra exposing (find)


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

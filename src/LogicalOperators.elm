module LogicalOperators exposing (..)


any : (a -> Bool) -> List a -> Bool
any isOkey list =
    case list of
        [] ->
            False

        x :: xs ->
            if isOkey x then
                True

            else
                any isOkey xs

any2 : (a -> Bool) -> List a -> Bool
any2 isOkey list =
    case list of 
        [] -> 
            False
        x :: xs ->
            isOkey x || any isOkey xs
module IfCascade exposing (..)


foobar : Bool -> Bool -> Bool
foobar foo bar =
    if foo then
        True

    else if not foo then
        False

    else
        False

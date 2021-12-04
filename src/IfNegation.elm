module IfNegation exposing (..)

notIf : Bool -> Bool
notIf a =
    if not a then
        True

    else
        False

notIf2 : Bool -> Bool
notIf2 a =
    if a |> not then
        True
    else 
        False
notIf3 : Bool -> Bool
notIf3 a =
    if not (a == True) then
        True
    else 
        False
module UnqualifiedImport exposing (..)

import List exposing (map)
import Html exposing (div)

a : List Int
a = map (\i -> i + 3) [1,2,3]
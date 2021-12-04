module UseLogicalOperatorsTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UseLogicalOperators exposing (rule)


all : Test
all =
    describe "UseLogicalOperators"
        [ test "should report an error when one path of an if expression returns a boolean value" <|
            \() ->
                source
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedError ]
        ]


expectedError : { message : String, details : List String, under : String }
expectedError =
    { message = "Use logical operators instead of if"
    , details =
        [ "When one path of an if expression returns a boolean value, then you can use a logical operator"
        , "For Example: \"if b then True else func x\" is the same as \"b || func x\", "
        ]
    , under = under
    }


under : String
under =
    """if isOkey x then
                True
            else 
                any isOkey xs"""


source : String
source =
    """
module Foo exposing (..)

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
"""

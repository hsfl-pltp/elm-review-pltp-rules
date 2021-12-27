module UseLogicalOperatorsTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UseLogicalOperators exposing (rule)


all : Test
all =
    describe "UseLogicalOperators"
        [ test "should report an error when the first path of an if expression returns a boolean value" <|
            \() ->
                invalidAndSource
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedAndError ]
        , test "should report an error when the second path of an if expression returns a boolean value" <|
            \() ->
                invalidOrSource
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedOrError ]
        , test "should not report an error when both baths return a boolean value" <|
            \() ->
                validSource
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


expectedAndError : { message : String, details : List String, under : String }
expectedAndError =
    { message = "Use a && operator instead of if"
    , details =
        [ "When the else path of an if expression returns a boolean value, you can use the && operator instead "
        , "For Example: \"if a then func b else False\" is the same as \"a && func b\", "
        ]
    , under = underAnd
    }


expectedOrError : { message : String, details : List String, under : String }
expectedOrError =
    { message = "Use a || operator instead of if"
    , details =
        [ "When the first path of an if expression returns a boolean valu, you can use the || operator instead"
        , "For Example: \"if a then True else func b\" is the same as \"a || func b\", "
        ]
    , under = underOr
    }


underAnd : String
underAnd =
    """if isOkey x then
                any isOkey xs

            else 
                False"""


underOr : String
underOr =
    """if isOkey x then
                True
            else 
                any isOkey xs"""


invalidAndSource : String
invalidAndSource =
    """
module Foo exposing (..)

any : (a -> Bool) -> List a -> Bool
any isOkey list =
    case list of 
        [] -> 
            False
        x :: xs ->
            if isOkey x then
                any isOkey xs

            else 
                False
"""


invalidOrSource : String
invalidOrSource =
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


validSource : String
validSource =
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
                False
"""

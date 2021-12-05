module NoUnnecessaryIfTest exposing (all)

import NoUnnecessaryIf exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "UnnecessaryIf"
        [ test "should report an error when an unnecessary if is detected" <|
            \() ->
                source
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedError
                        ]
        ]


expectedError : { message : String, details : List String, under : String }
expectedError =
    { message = "This is an unnecessary if."
    , details =
        [ "An if expression with True and False as results, the result is the expression itself."
        , "For Example: \"if b then True else False\" is the same as \"b\", "
        ]
    , under = "if bar then True else False"
    }


source : String
source =
    """
module Foo exposing (..)

foo : Bool -> Bool
foo bar =
    if bar then True else False
"""

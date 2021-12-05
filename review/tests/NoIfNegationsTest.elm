module NoIfNegationsTest exposing (all)

import NoIfNegations exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoIfNegations"
        [ test "should report an error when a not is used in a if expression" <|
            \() ->
                source
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedError ]
        ]


expectedError : { message : String, details : List String, under : String }
expectedError =
    { message = "This not in the if expression is unnecessary."
    , details =
        [ "Negation is not necessary when an expression is of the form \"if not a then b else c\""
        , "In this case you can remove the negation and swap b and c: \"if a then c else b\""
        ]
    , under = "bar |> not"
    }


source : String
source =
    """
module Foo exposing (..)

foo : Bool -> Bool
foo bar =
    if bar |> not then True else False
"""

module NoIfCascadeTest exposing (all)

import NoIfCascade exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoIfCascade"
        [ test "should report an error when REPLACEME" <|
            \() ->
                source
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedError
                        ]
        ]


expectedError : { message : String, details : List String, under : String }
expectedError =
    { message = "Cascading if expressions are not allowed"
    , details =
        [ "Cascading if expression are not needed, because you can solve this using logical operators like \"|| or &&\""
        ]
    , under = """if foo then
        if bar then
            True
        else 
            False
    else 
        False"""
    }


source : String
source =
    """
module Foo exposing (..)

foobar : Bool -> Bool -> Bool
foobar foo bar =
    if foo then
        if bar then
            True
        else 
            False
    else 
        False
"""

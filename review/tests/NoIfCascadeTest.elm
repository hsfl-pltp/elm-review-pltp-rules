module NoIfCascadeTest exposing (all)

import NoIfCascade exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoIfCascade"
        [ test "should report an error when a cascading if expression is found" <|
            \() ->
                invalidSource
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedError
                        ]
        , test "should not report an error when a else if is found" <|
            \() ->
                validSource
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors []
        ]


expectedError : { message : String, details : List String, under : String }
expectedError =
    { message = "Cascading if expressions are not allowed"
    , details =
        [ "Cascading if expression are not needed, because you can solve this using logical operators like \"|| or &&\""
        ]
    , under = under
    }


under : String
under =
    """if foo then
        if bar then
            True
        else 
            False
    else 
        False"""


invalidSource : String
invalidSource =
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


validSource : String
validSource =
    """
module Foo exposing (..)

foobar : Bool -> Bool -> Bool
foobar foo bar =
    if foo then
        True
    else if bar then  
        False
    else 
        False
"""

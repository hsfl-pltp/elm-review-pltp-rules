module NoMinimalRecordDestructingTest exposing (all)

import NoMinimalRecordDestructing exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMinimalRecordDestructing"
        [ test "should report an error when a minimal record destructing is used" <|
            \() ->
                source
                    |> Review.Test.run (rule 1)
                    |> Review.Test.expectErrors
                        [ Review.Test.error (expectedError 1) ]
        ]


expectedError : Int -> { message : String, details : List String, under : String }
expectedError threshold =
    { message = "Minimal record destructing detected."
    , details =
        [ "You used to few components from the record. You should use more then " ++ String.fromInt threshold ++ " components, or the function should have the needed components as arguments."
        , "For example: \"viewName { name } = ...\" should be implemented as \"viewName name = ... \"."
        ]
    , under = "{ name }"
    }


source : String
source =
    """
module Foo exposing (..)

type alias Person = 
    {name: String, age: Int}

viewName : Person -> String
viewName { name } =
    "The name of the person is:" ++ name
"""

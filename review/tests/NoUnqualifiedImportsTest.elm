module NoUnqualifiedImportsTest exposing (all)

import NoUnqualifiedImports exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "OnlyQualifiedImports"
        [ test "should report an error when a unqualified import is found" <|
            \() ->
                source
                    |> Review.Test.run (rule [ "Html", "Svg" ])
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedError
                        ]
        ]


expectedError : { message : String, details : List String, under : String }
expectedError =
    { message = "This is not an qualified import: map"
    , details =
        [ "A qualified import is a import, only exposing Types, like  \"import Foo exposing (MyCustomType)\""
        , "This make it easier to determine from which module the function is coming from."
        ]
    , under = "map"
    }


source : String
source =
    """
module Foo exposing(..)

import List exposing (map)
"""

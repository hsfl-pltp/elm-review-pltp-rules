module OnlyQualifiedImportsTest exposing (all)

import OnlyQualifiedImports exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "OnlyQualifiedImports"
        [ test "should report an error when a unqualified import is found" <|
            \() ->
                source
                    |> Review.Test.run (rule ["Html", "Svg"])
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedError
                        ]
        ]


expectedError : { message : String, details : List String, under : String }
expectedError =
    { message = "This is not an qualified import"
        , details =
            [ "A qualified import is something like 'import Foo'"
            , "and should be used like 'Foo.bar', to make clear, from which module 'bar' is coming from"
            ]
    , under = "import List exposing (map)"
    }


source : String
source =
    """
module Foo exposing(..)

import List exposing (map)
"""

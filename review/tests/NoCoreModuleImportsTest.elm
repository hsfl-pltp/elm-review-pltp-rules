module NoCoreModuleImportsTest exposing (all)

import NoCoreModuleImports exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "Test"
        [ test "should report an error when a core module is imported" <|
            \() ->
                source
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedError ]
        ]


expectedError : { message : String, details : List String, under : String }
expectedError =
    { message = "Import of core module found : List"
    , details =
        [ "The import of a core module is not necessary, because they are imported by default."
        , "For a list of all default imports take a look at https://package.elm-lang.org/packages/elm/core/latest/."
        ]
    , under = "import List exposing (map)"
    }


source : String
source =
    """
module A exposing(..)

import List exposing (map)
"""

module NoCoreModuleImportsTest exposing (all)

import NoCoreModuleImports exposing (rule)
import Review.Test exposing (ExpectedError)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoCoreModuleImports"
        [ testRule "Basics"
        , testRule "List"
        , testRule "Maybe"
        , testRule "Result"
        , testRule "String"
        , testRule "Char"
        , testRule "Tuple"
        , testRule "Debug"
        , testRule "Platform"
        , testRule "Platform.Cmd"
        , testRule "Platform.Sub"
        ]


testRule : String -> Test
testRule name =
    test ("Should report an error when the module " ++ name ++ "is imported.")
        (\_ ->
            source name
                |> Review.Test.run rule
                |> Review.Test.expectErrors [ assertError name ]
        )


assertError : String -> ExpectedError
assertError name =
    Review.Test.error
        { message = "Import of core module found : " ++ name
        , details =
            [ "The import of a core module is not necessary, because they are imported by default."
            , "For a list of all default imports take a look at https://package.elm-lang.org/packages/elm/core/latest/."
            ]
        , under = "import " ++ name ++ " exposing (..)"
        }


source : String -> String
source name =
    """
module A exposing(..)

import """ ++ name ++ " exposing (..)"

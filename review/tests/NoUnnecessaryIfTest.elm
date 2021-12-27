module NoUnnecessaryIfTest exposing (all)

import NoUnnecessaryIf exposing (rule)
import Review.Test exposing (ExpectedError)
import Test exposing (Test, describe)
import TestHelper


all : Test
all =
    describe "UnnecessaryIf"
        [ testRule "if bar then True else False"
        , testRule "if bar then False else True"
        ]


testRule : String -> Test
testRule expr =
    TestHelper.testRule ("Report " ++ expr ++ " Expression") (source expr) (expectedError expr) rule


expectedError : String -> ExpectedError
expectedError expr =
    Review.Test.error
        { message = "This is an unnecessary if."
        , details =
            [ "An if expression with True and False as results, the result is the expression itself."
            , "For Example: \"if b then True else False\" is the same as \"b\""
            , "But carefully, when the expression is \"if b then False else True\", the replacement is \"not b\""
            ]
        , under = expr
        }


source : String -> String
source expr =
    """
module Foo exposing (..)

foo : Bool -> Bool
foo bar =
    """ ++ expr

module NoBooleanComparisonTest exposing (all)

import NoBooleanComparison exposing (rule)
import Review.Test exposing (ExpectedError)
import Test exposing (Test, describe, test)
import TestHelper


all : Test
all =
    describe "NoBooleanComparison"
        [ testRule "bar == True"
        , testRule "bar == False"
        , testRule "True == bar"
        , testRule "False == bar"
        , testRule "True == (not bar)"
        , testRule "bar /= True"
        , testRule "bar /= False"
        , testRule "True /= bar"
        , testRule "False /= bar"
        , testRule "True /= (not bar)"
        ]


testRule : String -> Test
testRule expr =
    TestHelper.testRule
        ("Should report an error when a expression like " ++ expr ++ " is found.")
        (source expr)
        (assertError expr)
        rule


source : String -> String
source expr =
    """
module Foo exposing (..)

foo : Bool -> Bool
foo bar = 
    """ ++ expr


assertError : String -> ExpectedError
assertError under =
    Review.Test.error
        { message = "Detected a comparison with boolean"
        , details =
            [ "There is no need to compare a value of Type Boolean with \"True\" or \"False\""
            , "For Example: \"if b == True then .. else ..\" is the same as \"if b then ... else ...\", "
            ]
        , under = under
        }

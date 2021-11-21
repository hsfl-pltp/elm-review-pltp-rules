module NoBooleanComparisonTest exposing (all)

import NoBooleanComparison exposing (rule)
import Review.Test exposing (ExpectedError)
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoBooleanComparison"
        [ testRule "bar == True"
        , testRule "bar == False"
        , testRule "True == bar"
        , testRule "False == bar"
        ]


testRule : String -> Test
testRule expr =
    test ("Should report an error when " ++ expr ++ " is found.")
        (\_ ->
            source expr
                |> Review.Test.run rule
                |> Review.Test.expectErrors [ assertError expr ]
        )


source : String -> String
source expr =
    """
module Foo exposing (..)

foo : Bool -> Bool
foo bar = 
    if """ ++ expr ++ " then True else False"


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

module NoBooleanComparisonTest exposing (all)

import NoBooleanComparison exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoBooleanComparison"
        [ test "should report an error when a Value is compared to \"True\"" <|
            \() ->
                (source "bar == True")
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error (expectedError "bar == True") ]
        , test "should report an error when a \"True\" is compared to a value" <|
            \() ->
                (source "True == bar")
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error (expectedError "True == bar") ]
        , test "should report an error when a value is compared to \"False\"" <|
            \() ->
                (source "bar == False")
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error (expectedError "bar == False") ]
        , test "should report an error when a Boolean is compared to a value" <|
            \() ->
                (source "False == bar")
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error (expectedError "False == bar") ]
        ]


expectedError : String -> { message : String, details : List String, under : String }
expectedError under =
    { message = "Detected a comparison with boolean"
    , details =
        [ "There is no need to compare a value of Type Boolean with \"True\" or \"False\""
        , "For Example: \"if b == True then .. else ..\" is the same as \"if b then ... else ...\", "
        ]
    , under = under
    }


source : String -> String
source expr =
    """
module Foo exposing (..)

foo : Bool -> Bool
foo bar = 
    if """ ++ expr ++ " then True else False"

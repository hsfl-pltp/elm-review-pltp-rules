module NoNegationsTest exposing (all)

import NoNegations exposing (rule)
import Review.Test exposing (ExpectedError)
import Test exposing (Test, describe)
import TestHelper


all : Test
all =
    describe "NoIfNegations"
        [ testRule "not (a == b)" "a /= b"
        , testRule "not (a /= b)" "a == b"
        , testRule "not (a > b)" "a <= b"
        , testRule "not (a >= b)" "a < b"
        , testRule "not (a < b)" "a >= b"
        , testRule "not (a <= b)" "a > b"
        , testRule "not (a == 0 && 7 > b)" "a /= 0 || 7 <= b"
        , testRule "not (a == b || b >= a)" "a /= b && b < a"
        , testRule "not (a == True || b == False)" "a /= True && b /= False"
        ]


testRule : String -> String -> Test
testRule expr transformed =
    TestHelper.testRule
        ("Should report an error when a expression like " ++ expr ++ " is found.")
        (source expr)
        (assertError expr transformed)
        rule


assertError : String -> String -> ExpectedError
assertError under transformed =
    Review.Test.error
        { message = "Apply De Morgan's laws."
        , details =
            [ "When you apply De Morgan's laws, you dont need \"not\"."
            , "It can be rewritten as \"" ++ transformed ++ "\""
            ]
        , under = under
        }


source : String -> String
source expr =
    """
module Foo exposing (..)

foo : a -> a -> Bool
foo a b = 
    """ ++ expr

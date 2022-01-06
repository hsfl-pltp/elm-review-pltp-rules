module NoSinglePatternCaseTest exposing (all)

import NoSinglePatternCase exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoSingleConstructorCase"
        [ test "should report an error when a case expression has only one case" <|
            \() ->
                source
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ expectedError ]
        , test "should not report an error when a case expression has only one case" <|
            \() ->
                validSource
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


expectedError : Review.Test.ExpectedError
expectedError =
    Review.Test.error
        { message = "No case for one constructor types"
        , details =
            [ "When a custom type has only one constructor, you can move the pattern matching into the function defintion."
            , "For example: We define a type \"type Action = Action Int\". To get the value constructor you can write a function like \"run (Action x) = x\""
            ]
        , under = "Action x"
        }


source : String
source =
    """module Foo exposing (..)


f : Int -> Int
f i = i + 1

type Action 
    = Action Int

run : Action -> Int 
run action = 
    case action of 
        Action x ->
            x"""


validSource : String
validSource =
    """module Foo exposing (..)


f : Int -> Int
f i = i + 1

type Action 
    = Start Int
    | Stop

run : Action -> Int 
run action = 
    case action of 
        Start x -> 
            3
        Stop -> 
            0"""

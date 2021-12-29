module NoForbiddenFeaturesTest exposing (all)

import NoForbiddenFeatures exposing (Config, rule)
import Review.Test
import Test exposing (Test, describe, test)


config : Config
config =
    { operators = [ "|>" ]
    , functions = [ "List.map" ]
    , letIn = True
    , algebraicDataTypes = True
    , lambda = True
    }


all : Test
all =
    describe "NoForbiddenFeatures"
        [ test "should report an error when a forbidden feature is used" <|
            \() ->
                source
                    |> Review.Test.run (rule config)
                    |> Review.Test.expectErrors
                        [ Review.Test.error (expectedError "|>" "bar |> String.fromInt")
                        , Review.Test.error (expectedError "List.map" "List.map")
                        , Review.Test.error (expectedError "algebraic data type" "Value String")
                        , Review.Test.error (expectedError "let .. in .." "let a = toString list in a")
                        , Review.Test.error (expectedError "lambda expressions" "\\i -> String.fromInt e")
                        ]
        , test "should not report an error when no features is enabled" <|
            \() ->
                source
                    |> Review.Test.run (rule { operators = [], functions = [], letIn = False, algebraicDataTypes = False, lambda = False })
                    |> Review.Test.expectErrors []
        , test "should only report an error for letIn, when letIn is enabled" <|
            \() ->
                source
                    |> Review.Test.run (rule { operators = [], functions = [], letIn = True, algebraicDataTypes = False, lambda = False })
                    |> Review.Test.expectErrors
                        [ Review.Test.error (expectedError "let .. in .." "let a = toString list in a")
                        ]
        , test "should only report an error for lambda, when lambda is enabled" <|
            \() ->
                source
                    |> Review.Test.run (rule { operators = [], functions = [], letIn = False, algebraicDataTypes = False, lambda = True })
                    |> Review.Test.expectErrors
                        [ Review.Test.error (expectedError "lambda expressions" "\\i -> String.fromInt e")
                        ]
        , test "should only report an error for algebraicDataTypes, when algebraicDataTypes is enabled" <|
            \() ->
                source
                    |> Review.Test.run (rule { operators = [], functions = [], letIn = False, algebraicDataTypes = True, lambda = False })
                    |> Review.Test.expectErrors
                        [ Review.Test.error (expectedError "algebraic data type" "Value String")
                        ]
        ]


expectedError : String -> String -> { message : String, details : List String, under : String }
expectedError feature under =
    { message = "The use of " ++ feature ++ " is forbidden!"
    , details =
        [ "You have to solve the problem in another way..."
        ]
    , under = under
    }


source : String
source =
    """
module Foo exposing (..)

type MyType =
    Value String

foo : Int -> String
foo bar =
    bar |> String.fromInt

toString : List Int -> List String
toString list =
    List.map String.fromInt list


letIn : List Int -> List String 
letIn list =
    let a = toString list in a

withLambda : Int -> List
withLambda =
    (\\i -> String.fromInt e)
"""

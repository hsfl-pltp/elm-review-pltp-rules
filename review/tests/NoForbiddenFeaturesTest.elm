module NoForbiddenFeaturesTest exposing (all)

import NoForbiddenFeatures exposing (Config, rule)
import Review.Test
import Test exposing (Test, describe, test)


config : Config
config =
    { operators = [ "|>" ]
    , functions = [ "List.map" ]
    , letIn = True
    , productDataTypes = True
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
                        , Review.Test.error (expectedError "product data types" "Value String")
                        , Review.Test.error (expectedError "let .. in .." "let a = toString list in a")
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
"""

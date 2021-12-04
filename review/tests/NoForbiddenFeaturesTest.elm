module NoForbiddenFeaturesTest exposing (all)

import NoForbiddenFeatures exposing (Config, rule)
import Review.Test
import Test exposing (Test, describe, test)


config : Config
config =
    { operators = [ "|>" ]
    , functions = [ "List.map" ]
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

foo : Int -> String
foo bar =
    bar |> String.fromInt

toString : List Int -> List String
toString list =
    List.map String.fromInt list
"""

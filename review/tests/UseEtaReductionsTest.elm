module UseEtaReductionsTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UseEtaReductions exposing (rule)


all : Test
all =
    describe "UseEtaReductions"
        [ test "should report an error when a possible eta reduction is detected" <|
            \() ->
                functionSource
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error functionError ]
        , test "should report an error when a possible eta reduction for a lambda is detected" <|
            \() ->
                lambdaSource
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error lambdaError ]
        , test "should not report an errror" <|
            \() ->
                validSource
                    |> Review.Test.run rule
                    |> Review.Test.expectNoErrors
        ]


functionError : { message : String, details : List String, under : String }
functionError =
    { message = "Possible eta reduction detected."
    , details =
        [ "When the last argument of a function is the last applied to your expression, then you should remove both"
        , "Imagine you have a function with the signature incList : List Int -> List Int, with the implementation \"incList list = List.map inc list\""
        , "When you apply the eta reduction, you can remove the list argument and the last argument of the List.map function : \" incList = List.map inc\""
        ]
    , under = "List.map inc list"
    }


lambdaError : { message : String, details : List String, under : String }
lambdaError =
    { message = "Possible eta reduction for labmda detected."
    , details =
        [ "When the last argument of a lambda is the last applied to your epxression, then you should remove both"
        , "Iamgine you have a lambda like \"(\\e -> inc e\", then you can just write \"inc\""
        ]
    , under = "\\i -> inc i"
    }


functionSource : String
functionSource =
    """
module Foo exposing (..)

incList : List Int -> List Int
incList list = 
    List.map inc list
"""


lambdaSource : String
lambdaSource =
    """
module Foo exposing (..)

incList : List Int -> List Int
incList = 
    List.map (\\i -> inc i)
"""


validSource : String
validSource =
    """
module Foo exposing (..)

incList : List Int -> List Int
incList = 
    List.map inc
"""

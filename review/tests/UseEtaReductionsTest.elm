module UseEtaReductionsTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UseEtaReductions exposing (rule)


all : Test
all =
    describe "UseEtaReductions"
        [ test "should report an error when a possible eta reduction is detected" <|
            \() ->
                source
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedError ]
        ]


expectedError : { message : String, details : List String, under : String }
expectedError =
    { message = "Possible eta reduction detected."
    , details =
        [ "When the last argument of a function is the last applied to your expression, then you should remove both"
        , "Imagine you have a function with the signature incList : List Int -> List Int, with the implementation \"incList list = List.map inc list\""
        , "When you apply the eta reduction, you can remove the list argument and the last argument of the List.map function : \" incList = List.map inc\""
        ]
    , under = "List.map inc list"
    }


source : String
source =
    """
module Foo exposing (..)

incList : List Int -> List Int
incList list = 
    List.map inc list
"""

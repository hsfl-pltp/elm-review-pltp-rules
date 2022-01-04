module UseCommutingConversionsTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UseCommutingConversions exposing (rule)


all : Test
all =
    describe "UseCommutingConversions"
        [ test "should report an error for commuting conversions in if statement" <|
            \() ->
                sourceIf
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Possible commuting conversion for if detected"
                            , details =
                                [ "An expression in the form \"if a then f b else f c\" should be written as \"f(if a then b else c)\""
                                ]
                            , under = underIf
                            }
                        ]
        , test "Should report an error for commuting conversions in case" <|
            \() ->
                sourceCase
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "Possible commuting conversion for case detected"
                            , details =
                                [ "An expression in the form \"case e of p1 -> f e1 ... pn -> f en\" should be written as \"f (case e of p1 -> e1 ... pn -> en)\""
                                ]
                            , under = underCase
                            }
                        ]
        ]


underIf : String
underIf =
    """if b then 
        f 1
    else 
        f 2"""


sourceIf : String
sourceIf =
    """module Foo exposing (..)

f : Int -> Int
f i = i + 1

foo : Bool -> Int 
foo b =
    if b then 
        f 1
    else 
        f 2
"""


underCase : String
underCase =
    """case t of 
        One ->
            f 1
        Two ->
            f 2
        Three -> 
            f 3"""


sourceCase : String
sourceCase =
    """module Foo exposing (..)


f : Int -> Int
f i = i + 1


type CustomType
    = One
    | Two 
    | Three


foo : CustomType -> String 
foo t =
    case t of 
        One ->
            f 1
        Two ->
            f 2
        Three -> 
            f 3"""

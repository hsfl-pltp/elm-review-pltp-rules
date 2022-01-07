module UseCommutingConversionsTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UseCommutingConversions exposing (rule)


all : Test
all =
    describe "UseCommutingConversions"
        [ 
        --     test "should report an error for commuting conversions in if statement" <|
        --     \() ->
        --         sourceIf
        --             |> Review.Test.run rule
        --             |> Review.Test.expectErrors
        --                 [ Review.Test.error
        --                     { message = "Possible commuting conversion for if detected"
        --                     , details =
        --                         [ "An expression in the form \"if a then f b else f c\" should be written as \"f(if a then b else c)\""
        --                         ]
        --                     , under = underIf
        --                     }
        --                 ]
        -- , 
        test "Should report an error for commuting conversions in case" <|
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
    """case direction of
                Up ->
                    moveSnake Up model

                Down ->
                    moveSnake Down model

                Left ->
                    moveSnake Left model

                Right ->
                    moveSnake Right model"""


sourceCase : String
sourceCase =
    """module Foo exposing (..)


f : Int -> Int
f i = i + 1

type Action =
    Start Int

-- Should not be marked
destructAction : Action -> Int 
destructAction action = 
    case action of 
        Start x ->
            x


type CustomType
    = One
    | Two 
    | Three

update : Msg -> Model -> Model
update msg model =
    case msg of
        Msg direction ->
            case direction of
                Up ->
                    moveSnake Up model

                Down ->
                    moveSnake Down model

                Left ->
                    moveSnake Left model

                Right ->
                    moveSnake Right model
"""

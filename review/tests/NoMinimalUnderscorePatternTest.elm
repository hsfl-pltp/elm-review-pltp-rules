module NoMinimalUnderscorePatternTest exposing (all)

import NoMinimalUnderscorePattern exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMinimalUnderscorePattern"
        [ test "should report an error when the use of the underscore pattern covers to less cases" <|
            \() ->
                invalidSource
                    |> Review.Test.run (rule 4)
                    |> Review.Test.expectErrors
                        [ Review.Test.error expectedError ]
        , test "should not report an error when the underscore pattern covers enought cases" <|
            \() ->
                invalidSource
                    |> Review.Test.run (rule 1)
                    |> Review.Test.expectNoErrors
        , test "should not report an error when pattern match on a string" <|
            \() ->
                validSource
                    |> Review.Test.run (rule 3)
                    |> Review.Test.expectNoErrors
        ]


expectedError : { message : String, details : List String, under : String }
expectedError =
    { message = "To less covered cases by the underscore pattern, at lest 4 cases should be covered!"
    , details =
        [ "The underscore pattern should be used with care."
        , "You only covered 1 of 3 cases!"
        , "When you extend your custom type, then the new constructor will not be covered in the case expression."
        , "I suggest, when your custom type has less then 4 constructors, you should not use the underscore pattern"
        ]
    , under = under
    }


under : String
under =
    """case t of 
        One ->
            "First"
            
        _ -> 
            "Rest\""""


invalidSource : String
invalidSource =
    """module Foo exposing (..)

type CustomType
    = One
    | Two 
    | Three

foo : CustomType -> String 
foo t =
    case t of 
        One ->
            "First"
            
        _ -> 
            "Rest"
"""


validSource : String
validSource =
    """
module Foo exposing (..)

type Key
    = Up 
    | Down

foo : String -> Maybe Key 
foo string =
    case string of 
        "Up" ->
            Just Up
        "Down" ->
            Just Down
        _ -> 
            Nothing

bar : Maybe String -> String 
bar maybe = 
    case maybe of 
        Just "Hello" ->
            "Hi"

        Just _ ->
            "Something"

        _ -> 
            ""
"""

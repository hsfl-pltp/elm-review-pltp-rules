module NoMinimalUnderscorePatternTest exposing (all)

import NoMinimalUnderscorePattern exposing (rule)
import Review.Test
import Test exposing (Test, describe, test)


all : Test
all =
    describe "NoMinimalUnderscorePattern"
        [ test "should report an error when the use of the underscore pattern covers to less cases" <|
            \() ->
                source
                    |> Review.Test.run (rule 3)
                    |> Review.Test.expectErrors
                        [ Review.Test.error
                            { message = "To less covered cases by the underscore pattern, at lest 3 cases should be covered!"
                            , details =
                                [ "The underscore pattern should be used with care."
                                , "You only covered 1 of 5 cases!"
                                , "When you extend your custom type, then the new constructor will not be covered in the case expression."
                                , "I suggest, when your custom type has less then 3 constructors, you should not use the underscore pattern"
                                ]
                            , under = "_"
                            }
                        ]
        ]


source : String
source =
    """module Foo exposing (..)

type CustomType
    = One
    | Two 
    | Three
    | Four 
    | Five

foo : CustomType -> String 
foo t =
    case t of 
        One ->
            "First"
        _ ->
            "Rest"

"""

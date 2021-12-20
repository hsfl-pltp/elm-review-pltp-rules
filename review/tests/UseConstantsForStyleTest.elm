module UseConstantsForStyleTest exposing (all)

import Review.Test
import Test exposing (Test, describe, test)
import UseConstantsForStyle exposing (rule)


all : Test
all =
    describe "UseConstantsForStyle"
        [ test "should report an error when at least 2 style attributes used, in one html element" <|
            \() ->
               source
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ Review.Test.error (expectedError """[style "padding" "0", style "margin" "0"]""")
                        ]
        ]


expectedError : String -> { message : String, details : List String, under : String }
expectedError under =
    { message = "Group styles into a constant"
    , details =
        [ "To structure your code, you should use a constant for styles"
        , "Move all attributes for style, into a constant. For Example \"myStyle = [ style ..., style...]\""
        ]
    , under = under
    }


source : String
source =
    """
module Foobar exposing (..)

import Html exposing(Html, div)
import Html.Attributes exposing (style)

viewContainer : Html msg
viewContainer =
    div [style "padding" "0", style "margin" "0"] []
    """

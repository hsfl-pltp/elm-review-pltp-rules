module UseNamingConventionsTest exposing (all)

import Review.Test exposing (ExpectedError)
import Test exposing (Test, describe, test)
import UseNamingConventions exposing (rule)


all : Test
all =
    describe "UseCorrectNaming"
        [ test "should report an error when REPLACEME" <|
            \() ->
                source
                    |> Review.Test.run rule
                    |> Review.Test.expectErrors
                        [ expectedError "getAge" ]
        ]


expectedError : String -> ExpectedError
expectedError name =
    Review.Test.error
        { message = "This is not a good function name: " ++ name
        , details =
            [ "The name get, comes from imperative programming, a more expressive name would be without get"
            ]
        , under = name
        }


source : String
source =
    """
module Foo exposing (..)

type alias Person =
    {age : Int}

getAge { age } = age 
    """

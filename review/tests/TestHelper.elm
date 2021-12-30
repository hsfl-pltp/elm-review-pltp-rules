module TestHelper exposing (testRule, testRuleNoErrors)

import Review.Rule exposing (Rule)
import Review.Test exposing (ExpectedError)
import Test exposing (Test, test)


testRule : String -> String -> ExpectedError -> Rule -> Test
testRule title source error rule =
    test title
        (\_ ->
            source
                |> Review.Test.run rule
                |> Review.Test.expectErrors [ error ]
        )

testRuleNoErrors : String -> String -> Rule -> Test
testRuleNoErrors title source rule =
    test title
        (\_ ->
            source
                |> Review.Test.run rule
                |> Review.Test.expectNoErrors
        )
module UnnecessaryIf exposing (rule)

{-| Forbids the use of unnecessary if statements

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)
import Elm.Syntax.Expression exposing (Expression)


{-| Reports the use of unnecessary if statements

    config =
        [ UnnecessaryIf.rule
        ]


## Fail

foo : Bool -> Bool 
foo bar = 
    if bar then 
        True
    else 
        False


## Success

foo : Bool -> Bool 
foo bar = 
    bar

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "UnnecessaryIf" ()
        -- Add your visitors
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node = 
    case Node.value node of
        Expression.IfBlock _ left right ->
            validateIf node left right
        _ ->
            []

validateIf : Node Expression -> Node Expression -> Node Expression -> List (Error {})
validateIf node left right =
    if matchExpression left "True" && matchExpression right "False" then
        ruleErrors node 
    else 
        []

matchExpression : Node Expression -> String -> Bool
matchExpression node expected =
    case Node.value node of
        Expression.FunctionOrValue [] value -> 
            value == expected

        _ -> 
            False

ruleErrors : Node Expression -> List (Error {})
ruleErrors node =
    [ Rule.error
        { message = "This is an unnecessary if."
        , details =
            [ "An if expression with True and False as results, the result is the expression itself."
            , "For Example: \"if b then True else False\" is the same as \"b\", "
            ]
        }
        (Node.range node)
    ]
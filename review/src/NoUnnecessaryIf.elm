module NoUnnecessaryIf exposing (rule)

{-| Forbids the use of unnecessary if statements

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


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

foo2 : Bool -> Bool
foo2 bar =
if bar then
False

    else
        True


## Success

foo : Bool -> Bool
foo bar =

    bar

foo2 : Bool -> Bool
foo2 bar =
not bar

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
            errorsForIf node left right ++ errorsForIf node right left

        _ ->
            []


errorsForIf : Node Expression -> Node Expression -> Node Expression -> List (Error {})
errorsForIf parent left right =
    if matchExpression left "True" && matchExpression right "False" then
        [ ifError parent ]

    else
        []


matchExpression : Node Expression -> String -> Bool
matchExpression node expected =
    case Node.value node of
        Expression.FunctionOrValue [] value ->
            value == expected

        _ ->
            False


ifError : Node Expression -> Error {}
ifError node =
    Rule.error
        { message = "This is an unnecessary if."
        , details =
            [ "An if expression with True and False as results, the result is the expression itself."
            , "For Example: \"if b then True else False\" is the same as \"b\""
            , "But carefully, when the expression is \"if b then False else True\", the replacement is \"not b\""
            ]
        }
        (Node.range node)

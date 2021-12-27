module NoBooleanComparison exposing (rule)

{-| Forbids the comparison with booleans

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the comparison with booleans

    config =
        [ NoBooleanComparison.rule
        ]


## Fail

    if bar == True then
        ...
    else
        ...

    if bar == False then
        ...
    else
        ...


## Success

    if bar then
        ...
    else
        ...

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoBooleanComparison" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.OperatorApplication "==" _ left right ->
            errorsForOperator node left right

        Expression.OperatorApplication "/=" _ left right ->
            errorsForOperator node left right

        _ ->
            []


errorsForOperator : Node Expression -> Node Expression -> Node Expression -> List (Error {})
errorsForOperator node left right =
    if
        matchExpression left "True"
            || matchExpression left "False"
            || matchExpression right "True"
            || matchExpression right "False"
    then
        [ruleError node]

    else
        []


matchExpression : Node Expression -> String -> Bool
matchExpression node expected =
    case Node.value node of
        Expression.FunctionOrValue [] value ->
            value == expected

        _ ->
            False


ruleError : Node Expression -> Error {}
ruleError node =
    Rule.error
        { message = "Detected a comparison with boolean"
        , details =
            [ "There is no need to compare a value of Type Boolean with \"True\" or \"False\""
            , "For Example: \"if b == True then .. else ..\" is the same as \"if b then ... else ...\", "
            ]
        }
        (Node.range node)

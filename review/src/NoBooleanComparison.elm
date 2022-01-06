module NoBooleanComparison exposing (rule)

{-| Forbids the comparison with booleans

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Helper
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
            errorsForOperator node (Node.value left) (Node.value right)

        Expression.OperatorApplication "/=" _ left right ->
            errorsForOperator node (Node.value left) (Node.value right)

        _ ->
            []


errorsForOperator : Node Expression -> Expression -> Expression -> List (Error {})
errorsForOperator node left right =
    if Helper.isBoolExpression left && not (Helper.isBoolExpression right) then
        [ ruleError node ]

    else if not (Helper.isBoolExpression left) && Helper.isBoolExpression right then
        [ ruleError node ]

    else
        []


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

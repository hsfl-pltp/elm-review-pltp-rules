module NoIfNegations exposing (rule)

{-| Forbids the use of negations in if

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when the use of negations in if expressions are detected

    config =
        [ NoIfNegations.rule
        ]


## Fail

    if not a then
        b

    else
        c


## Success

    if a then
        c

    else
        b

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoIfNegations" ()
        -- Add your visitors
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.IfBlock expression _ _ ->
            validateExpression expression

        _ ->
            []


validateExpression : Node Expression -> List (Error {})
validateExpression node =
    case Node.value node of
        Expression.Application list ->
            validateApplication node list

        Expression.OperatorApplication "|>" _ _ expression ->
            if matchExpression expression "not" then
                ruleErrors node

            else
                []

        _ ->
            []


validateApplication : Node Expression -> List (Node Expression) -> List (Error {})
validateApplication node list =
    case list of
        [] ->
            []

        x :: xs ->
            if matchExpression x "not" then
                ruleErrors node ++ validateApplication node xs

            else
                validateApplication node xs


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
        { message = "This not in the if expression is unnecessary."
        , details =
            [ "Negation is not necessary when an expression is of the form \"if not a then b else c\""
            , "In this case you can remove the negation and swap b and c: \"if a then c else b\""
            ]
        }
        (Node.range node)
    ]

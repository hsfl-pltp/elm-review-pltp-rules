module NoIfCascade exposing (rule)

{-| Forbids the use of if cascade

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression, isIfElse)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of cascading if expressions

    config =
        [ NoIfCascade.rule
        ]


## Fail

    a =
        if a then
            if b then
                ...
            else
                ...
        else
            ...


## Success

    a =
        if a then
            ...
        else
            ...

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoIfCascade" ()
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
    if isIfElse left || isIfElse right then
        ruleErrors node

    else
        []


isIfElse : Node Expression -> Bool
isIfElse (Node _ node) =
    Expression.isIfElse node


ruleErrors : Node Expression -> List (Error {})
ruleErrors node =
    [ Rule.error
        { message = "Cascading if expressions are not allowed"
        , details =
            [ "Cascading if expression are not needed, because you can solve this using logical operators like \"|| or &&\""
            ]
        }
        (Node.range node)
    ]

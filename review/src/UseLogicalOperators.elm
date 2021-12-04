module UseLogicalOperators exposing (rule)

{-| Forbids the use of if, when one path returns a bool value

@docs rule

-}

import Review.Rule as Rule exposing (Rule, Error)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Expression as Expression exposing(Expression)
{-| Reports an error when one path of en if expression returns a bool value

    config =
        [ UseLogicalOperators.rule
        ]


## Fail


    any : (a -> Bool) -> List a -> Bool
    any isOkey list =
        case list of 
            [] -> 
                False
            x :: xs ->
                if isOkey x then
                    True
                else 
                    any isOkey xs




## Success

    any : (a -> Bool) -> List a -> Bool
    any isOkey list =
        case list of 
            [] -> 
                False
            x :: xs ->
                isOkey x || any isOkey xs

```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "UseLogicalOperators" ()
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
    if matchExpression left "True" 
        || matchExpression left "False"
        || matchExpression right "True"
        || matchExpression right "False"
        then
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
        { message = "Use logical operators instead of if"
        , details =
            [ "When one path of an if expression returns a boolean value, then you can use a logical operator"
            , "For Example: \"if b then True else func x\" is the same as \"b || func x\", "
            ]
        }
        (Node.range node)
    ]
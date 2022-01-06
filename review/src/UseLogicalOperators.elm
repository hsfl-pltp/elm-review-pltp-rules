module UseLogicalOperators exposing (rule)

{-| Forbids the use of if, when one path returns a bool value

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Helper
import Review.Rule as Rule exposing (Error, Rule)


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
            errorsForIf node (Node.value left) (Node.value right)

        _ ->
            []


errorsForIf : Node Expression -> Expression -> Expression -> List (Error {})
errorsForIf parent left right =
    if not (Helper.isBoolExpression left) && Helper.isBoolExpression right then
        [ andError parent ]

    else if Helper.isBoolExpression left && not (Helper.isBoolExpression right) then
        [ orError parent ]

    else
        []


andError : Node Expression -> Error {}
andError node =
    Rule.error
        { message = "Use a && operator instead of if"
        , details =
            [ "When the else path of an if expression returns a boolean value, you can use the && operator instead "
            , "For Example: \"if a then func b else False\" is the same as \"a && func b\", "
            ]
        }
        (Node.range node)


orError : Node Expression -> Error {}
orError node =
    Rule.error
        { message = "Use a || operator instead of if"
        , details =
            [ "When the first path of an if expression returns a boolean valu, you can use the || operator instead"
            , "For Example: \"if a then True else func b\" is the same as \"a || func b\", "
            ]
        }
        (Node.range node)

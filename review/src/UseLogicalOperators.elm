module UseLogicalOperators exposing (rule)

{-| Forbids the use of if, when one path returns a bool value

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
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
            errorsForIf node left right

        _ ->
            []


errorsForIf : Node Expression -> Node Expression -> Node Expression -> List (Error {})
errorsForIf parent left right =
    errorsForAnd parent left right ++ errorsForOr parent left right


errorsForAnd : Node Expression -> Node Expression -> Node Expression -> List (Error {})
errorsForAnd node left right =
    if not (isBooleanExpression left) && isBooleanExpression right then
        [ andError node ]

    else
        []


errorsForOr : Node Expression -> Node Expression -> Node Expression -> List (Error {})
errorsForOr node left right =
    if isBooleanExpression left && not (isBooleanExpression right) then
        [ orError node ]

    else
        []


isBooleanExpression : Node Expression -> Bool
isBooleanExpression (Node _ node) =
    case node of
        Expression.FunctionOrValue [] value ->
            value == "True" || value == "False"

        _ ->
            False


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

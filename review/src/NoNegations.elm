module NoNegations exposing (rule)

{-| Forbids the use of negations in if

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when the use of negations in if expressions are detected

    config =
        [ NoIfNegations.rule
        ]


## Fail

    not (a == b)

    not (a /= b)

    not (a > b)

    not (a >= b)

    not (a < b)

    not (a <= b)


## Success

    a /= b

    a == b

    a <= b

    a < b

    a >= b

    a > b

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoNegations" ()
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.Application nodes ->
            errorsForApplication node (List.map Node.value nodes)

        _ ->
            []


errorsForApplication : Node Expression -> List Expression -> List (Error {})
errorsForApplication parent list =
    case list of
        [] ->
            []

        (Expression.FunctionOrValue [] "not") :: xs ->
            errorsForNot parent xs

        _ :: xs ->
            errorsForApplication parent xs


errorsForNot : Node Expression -> List Expression -> List (Error {})
errorsForNot parent list =
    case list of
        (Expression.OperatorApplication operator _ left right) :: [] ->
            [ notError parent (transformOperatorApplication operator left right)
            ]

        (Expression.ParenthesizedExpression (Node _ expr)) :: xs ->
            errorsForNot parent (expr :: xs)

        _ ->
            []


transform : Node Expression -> String
transform expression =
    case Node.value expression of
        Expression.OperatorApplication operator _ left right ->
            transformOperatorApplication operator left right

        Expression.ParenthesizedExpression expr ->
            " ( " ++ transform expr ++ " ) "

        Expression.RecordAccess expr (Node _ name) ->
            transform expr ++ "." ++ name

        Expression.FunctionOrValue [] name ->
            name

        Expression.Integer int ->
            String.fromInt int

        Expression.Floatable float ->
            String.fromFloat float

        Expression.Literal string ->
            string

        _ ->
            ""


transformOperatorApplication : String -> Node Expression -> Node Expression -> String
transformOperatorApplication operator left right =
    String.join " " [ transform left, transformOperator operator, transform right ]


transformOperator : String -> String
transformOperator operator =
    case operator of
        "&&" ->
            "||"

        "||" ->
            "&&"

        "==" ->
            "/="

        "/=" ->
            "=="

        ">" ->
            "<="

        "<" ->
            ">="

        "<=" ->
            ">"

        ">=" ->
            "<"

        _ ->
            operator


notError : Node Expression -> String -> Error {}
notError node transformed =
    Rule.error
        { message = "Apply De Morgan's laws."
        , details =
            [ "When you apply De Morgan's laws, you dont need \"not\"."
            , "It can be rewritten as \"" ++ transformed ++ "\""
            ]
        }
        (Node.range node)

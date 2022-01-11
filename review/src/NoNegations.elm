module NoNegations exposing (rule)

{-| Forbids the use of not ()

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.Infix exposing (InfixDirection(..))
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports when the use of not

    config =
        [ NoIfNegations.rule
        ]


## Fail

    not (a && b)

    not (a || b)

    not (a == b)

    not (a /= b)

    not (a > b)

    not (a >= b)

    not (a < b)

    not (a <= b)


## Success

    not a || not b

    not a && not b

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
        (Expression.FunctionOrValue [] "not") :: expr :: _ ->
            errorsForNot parent expr

        _ ->
            []


errorsForNot : Node Expression -> Expression -> List (Error {})
errorsForNot parent expression =
    case expression of
        Expression.ParenthesizedExpression (Node _ expr) ->
            errorsForOperator parent expr

        _ ->
            []


errorsForOperator : Node Expression -> Expression -> List (Error {})
errorsForOperator parent expr =
    case expr of
        Expression.OperatorApplication operator _ left right ->
            [ notError parent (String.join " " (negateOperator operator left right)) ]

        _ ->
            []


negateOperator : String -> Node Expression -> Node Expression -> List String
negateOperator operator left right =
    if operator == "||" || operator == "&&" then
        case ( Node.value left, Node.value right ) of
            ( Expression.FunctionOrValue [] leftName, Expression.FunctionOrValue [] rightName ) ->
                [ "not", leftName, transformOperator operator, "not", rightName ]

            ( Expression.FunctionOrValue [] leftName, _ ) ->
                [ "not", leftName, transformOperator operator, transform right ]

            ( _, Expression.FunctionOrValue [] rightName ) ->
                [ transform left, transformOperator operator, "not", rightName ]

            _ ->
                [ transform left, transformOperator operator, transform right ]

    else
        [ transform left, transformOperator operator, transform right ]


transform : Node Expression -> String
transform expression =
    case Node.value expression of
        Expression.OperatorApplication operator _ left right ->
            String.join " " (negateOperator operator left right)

        Expression.ParenthesizedExpression expr ->
            "(" ++ transform expr ++ ")"

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

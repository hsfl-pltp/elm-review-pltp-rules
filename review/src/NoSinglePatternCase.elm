module NoSinglePatternCase exposing (rule)

{-| Forbids the use of case expressions for a custom type with only one constructor

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern exposing (Pattern)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of case expressions for a custom type with only one constructor

    config =
        [ NoSingleConstructorCase.rule
        ]


## Fail

    type Action
        = Action Int

    a action =
        case action of
            Action x ->
                x


## Success

    a (Action x) =
        x

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoSinglePatternCase" ()
        -- Add your visitors
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            errorsForCases (List.map Tuple.first cases)

        _ ->
            []


errorsForCases : List (Node Pattern) -> List (Error {})
errorsForCases list =
    case list of
        [ pattern ] ->
            [ patternError pattern ]

        _ ->
            []


patternError : Node Pattern -> Error {}
patternError node =
    Rule.error
        { message = "No case for one constructor types"
        , details =
            [ "When a custom type has only one constructor, you can move the pattern matching into the function defintion."
            , "For example: We define a type \"type Action = Action Int\". To get the value constructor you can write a function like \"run (Action x) = x\""
            ]
        }
        (Node.range node)

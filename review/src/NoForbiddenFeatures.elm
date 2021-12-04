module NoForbiddenFeatures exposing
    ( rule
    , Config
    )

{-| Forbids the use of functions and features, defined by configuration

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of forbidden features and functions

    config =
        [ NoForbiddenFeatures.rule
            { operators = [ "|>", "<|" ]
            , functions = [ "List.map", "List.foldr" ]
            }
        ]


## Example for operators


### Fail

    foo bar =
        bar |> func


### Success

    foo bar =
        func bar


## Example for functions

    foo bar =
        List.map func bar

    foo bar =
        case bar of
            ...
            ...

-}
type alias Config =
    { operators : List String
    , functions : List String
    }


rule : Config -> Rule
rule config =
    Rule.newModuleRuleSchema "NoForbiddenFeatures" ()
        |> Rule.withSimpleExpressionVisitor (expressionVisitor config)
        |> Rule.fromModuleRuleSchema


expressionVisitor : Config -> Node Expression -> List (Error {})
expressionVisitor config node =
    case Node.value node of
        Expression.OperatorApplication operator _ _ _ ->
            validateFeature config.operators node operator

        Expression.FunctionOrValue mod func ->
            validateFeature config.functions node (functionName mod func)

        _ ->
            []


functionName : List String -> String -> String
functionName moduleName func =
    String.join "." (moduleName ++ [ func ])


validateFeature : List String -> Node Expression -> String -> List (Error {})
validateFeature forbidden node feature =
    if List.member feature forbidden then
        [ ruleErrors feature node ]

    else
        []


ruleErrors : String -> Node Expression -> Error {}
ruleErrors feature node =
    Rule.error
        { message = "The use of " ++ feature ++ " is forbidden!"
        , details =
            [ "You have to solve the problem in another way..."
            ]
        }
        (Node.range node)

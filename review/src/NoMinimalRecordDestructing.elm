module NoMinimalRecordDestructing exposing (rule)

{-| Forbids the use of record destructing, when only a few components from the record is used

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of record destructing, where only a few components are used from the record

    config =
        [ NoMinimalRecordDestructing.rule
        ]


## Fail

    type alias MyRecord = {age : Int, name : String}

    viewAge { age } =
        ...


## Success

    viewAge age =
        ...

-}
rule : Int -> Rule
rule threshold =
    Rule.newModuleRuleSchema "NoMinimalRecordDestructing" ()
        -- Add your visitors
        |> Rule.withSimpleDeclarationVisitor (declarationVisitor threshold)
        |> Rule.fromModuleRuleSchema


declarationVisitor : Int -> Node Declaration -> List (Error {})
declarationVisitor threshold node =
    case Node.value node of
        Declaration.FunctionDeclaration fn ->
            validateFunction threshold fn

        _ ->
            []


validateFunction : Int -> Function -> List (Error {})
validateFunction threshold { declaration } =
    declaration
        |> Node.value
        |> .arguments
        |> validateArguments threshold


validateArguments : Int -> List (Node Pattern) -> List (Error {})
validateArguments threshold list =
    case list of
        [] ->
            []

        x :: xs ->
            validateArgument threshold x ++ validateArguments threshold xs


validateArgument : Int -> Node Pattern -> List (Error {})
validateArgument threshold pattern =
    case Node.value pattern of
        Pattern.RecordPattern listUsed ->
            if inThreshold threshold listUsed then
                ruleErrors threshold pattern

            else
                []

        _ ->
            []


inThreshold : Int -> List (Node String) -> Bool
inThreshold threshold list =
    List.length list <= threshold


ruleErrors : Int -> Node Pattern -> List (Error {})
ruleErrors threshold node =
    [ Rule.error
        { message = "Minimal record destructing detected."
        , details =
            [ "You used to few components from the record. You should use more then " ++ String.fromInt threshold ++ " components, or the function should have the needed components as arguments."
            , "For example: \"viewName { name } = ...\" should be implemented as \"viewName name = ... \"."
            ]
        }
        (Node.range node)
    ]

module NoMinimalRecordDestructing exposing (rule)

{-| Forbids the use of record destructing, when only a few components from the record is used

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Node as Node exposing (Node)
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of record destructing, where only a few components are used from the record

    config =
        [ NoMinimalRecordDestructing.rule 1
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
        |> Rule.withSimpleDeclarationVisitor (declarationVisitor threshold)
        |> Rule.fromModuleRuleSchema


declarationVisitor : Int -> Node Declaration -> List (Error {})
declarationVisitor threshold node =
    case Node.value node of
        Declaration.FunctionDeclaration fn ->
            fn
                |> arguments
                |> List.concatMap (errorForArgument threshold)

        _ ->
            []


arguments : Function -> List (Node Pattern)
arguments { declaration } =
    .arguments (Node.value declaration)


errorForArgument : Int -> Node Pattern -> List (Error {})
errorForArgument threshold pattern =
    case Node.value pattern of
        Pattern.RecordPattern listUsed ->
            if List.length listUsed <= threshold then
                [ ruleError threshold pattern ]

            else
                []

        _ ->
            []


ruleError : Int -> Node Pattern -> Error {}
ruleError threshold node =
    Rule.error
        { message = "Minimal record destructing detected."
        , details =
            [ "You used to few components from the record. You should use more then " ++ String.fromInt threshold ++ " components, or the function should have the needed components as arguments."
            , "For example: \"viewName { name } = ...\" should be implemented as \"viewName name = ... \"."
            ]
        }
        (Node.range node)

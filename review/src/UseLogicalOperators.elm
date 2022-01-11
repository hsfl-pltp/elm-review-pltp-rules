module UseLogicalOperators exposing (rule)

{-| Forbids the use of if, when one path returns a bool value

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Helper
import Review.Rule as Rule exposing (Error, Rule)
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)


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
initialContext : Rule.ContextCreator () ModuleNameLookupTable
initialContext =
    Rule.initContextCreator
        (\lookupTable () -> lookupTable)
        |> Rule.withModuleNameLookupTable

rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "UseLogicalOperators" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> ModuleNameLookupTable -> (List (Error {}), ModuleNameLookupTable)
expressionVisitor node lookupTable =
    case Node.value node of
        Expression.IfBlock _ left right ->
            (errorsForIf node left right lookupTable, lookupTable)

        _ ->
            ([], lookupTable)


errorsForIf : Node Expression -> Node Expression -> Node Expression -> ModuleNameLookupTable-> List (Error {})
errorsForIf parent left right lookupTable=
    if not (Helper.isBoolExpression left lookupTable) && Helper.isBoolExpression right lookupTable then
        [ andError parent ]

    else if Helper.isBoolExpression left lookupTable && not (Helper.isBoolExpression right lookupTable) then
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

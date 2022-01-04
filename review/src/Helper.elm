module Helper exposing (functionName, isBoolExpression, toModuleName)

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node(..))
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)


functionName : Node a -> ModuleNameLookupTable -> String -> String
functionName node lookupTable func =
    case ModuleNameLookupTable.moduleNameFor lookupTable node of
        Nothing ->
            func

        Just name ->
            toModuleName name ++ "." ++ func


toModuleName : ModuleName -> String
toModuleName =
    String.join "."


isBoolExpression : Node Expression -> Bool
isBoolExpression (Node _ node) =
    case node of
        Expression.FunctionOrValue [] value ->
            value == "True" || value == "False"

        _ ->
            False

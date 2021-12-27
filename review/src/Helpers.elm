module Helpers exposing (functionName, moduleName)

import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node exposing (Node)
import Review.ModuleNameLookupTable as ModuleNameLookupTable exposing (ModuleNameLookupTable)


functionName : Node a -> ModuleNameLookupTable -> String -> String
functionName node lookupTable func =
    case ModuleNameLookupTable.moduleNameFor lookupTable node of
        Nothing ->
            func

        Just name ->
            moduleName name ++ "." ++ func


moduleName : ModuleName -> String
moduleName =
    String.join "."

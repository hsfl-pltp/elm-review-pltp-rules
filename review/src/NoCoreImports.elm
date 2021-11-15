module NoCoreImports exposing (rule)

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


rule : Rule
rule =
    Rule.newModuleRuleSchema "NoCoreImports" ()
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.fromModuleRuleSchema


blackList : List String
blackList =
    [ "List" ]


importVisitor : Node Import -> List (Error {})
importVisitor node =
    errorsForCoreImport node (Node.value node)


errorsForCoreImport : Node Import -> Import -> List (Error {})
errorsForCoreImport node { moduleName } =
    checkModuleName node (Node.value moduleName)


checkModuleName : Node Import -> List String -> List (Error {})
checkModuleName node list =
    let 
        moduleName : String
        moduleName = String.join "." list
    in
    if List.member moduleName blackList then
        [ Rule.error
            { message = "Import of Core module found: " ++ moduleName
            , details =
                [ "The import of core modules are not necessary, because they are imported by default."
                , "Take a look at https://package.elm-lang.org/packages/elm/core/latest to see a list of all default core imports."
                ]
            }
            (Node.range node)
        ]

    else
        []

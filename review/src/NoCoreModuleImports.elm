module NoCoreModuleImports exposing (rule)

{-| Forbids the import of core modules, which are default imports.

@docs rule

-}

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the import of core modules

    config =
        [ NoCoreModuleImports.rule
        ]


## Fail

    import List exposing (map)

    a =
        map (\e -> e + 1) [ 1, 2, 3 ]


## Success

    a =
        List.map (\e -> e + 1) [ 1, 2, 3 ]

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoCoreModuleImports" ()
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.fromModuleRuleSchema


coreModules : List String
coreModules =
    [ "Basics"
    , "List"
    , "Maybe"
    , "Result"
    , "String"
    , "Char"
    , "Tuple"
    , "Debug"
    , "Platform"
    , "Platform.Cmd"
    , "Platform.Sub"
    ]


importVisitor : Node Import -> List (Error {})
importVisitor node =
    validateImport node (toModuleName node)


validateImport : Node Import -> String -> List (Error {})
validateImport node moduleName =
    if List.member moduleName coreModules then
        [ ruleError node moduleName ]

    else
        []


toModuleName : Node Import -> String
toModuleName (Node _ { moduleName }) =
    String.join "." (Node.value moduleName)


ruleError : Node Import -> String -> Error {}
ruleError node name =
    Rule.error
        { message = "Import of core module found : " ++ name
        , details =
            [ "The import of a core module is not necessary, because they are imported by default."
            , "For a list of all default imports take a look at https://package.elm-lang.org/packages/elm/core/latest/."
            ]
        }
        (Node.range node)

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
    import String exposing (contains)


## Success

    -- No Import of List and String



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
    errorsForImport node (toModuleName node)


errorsForImport : Node Import -> String -> List (Error {})
errorsForImport node moduleName =
    if List.member moduleName coreModules then
        [ importError node moduleName ]

    else
        []


toModuleName : Node Import -> String
toModuleName (Node _ { moduleName }) =
    String.join "." (Node.value moduleName)


importError : Node Import -> String -> Error {}
importError node name =
    Rule.error
        { message = "Import of core module found : " ++ name
        , details =
            [ "The import of a core module is not necessary, because they are imported by default."
            , "For a list of all default imports take a look at https://package.elm-lang.org/packages/elm/core/latest/."
            ]
        }
        (Node.range node)

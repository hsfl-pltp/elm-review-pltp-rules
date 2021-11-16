module NoCoreModuleImports exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports... REPLACEME

    config =
        [ NoCoreModuleImports.rule
        ]


## Fail

    a =
        "REPLACEME example to replace"


## Success

    a =
        "REPLACEME example to replace"


## When (not) to enable this rule

This rule is useful when REPLACEME.
This rule is not useful when REPLACEME.


## Try it out

You can try this rule out by running the following command:

```bash
elm-review --template undefined/example --rules NoCoreModuleImports
```

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "NoCoreModuleImports" ()
        -- Add your visitors
        |> Rule.withSimpleImportVisitor importVisitor
        |> Rule.fromModuleRuleSchema


coreModules : List String
coreModules =
    [ "List" ]


isCoreModule : String -> Bool
isCoreModule name =
    List.member name coreModules


moduleName : Import -> String
moduleName i =
    String.join "." (Node.value i.moduleName)


importVisitor : Node Import -> List (Error {})
importVisitor node =
    let
        name : String
        name =
            moduleName (Node.value node)
    in
    if isCoreModule name then
        ruleErrors node name

    else
        []


ruleErrors : Node Import -> String -> List (Error {})
ruleErrors node name =
    [ Rule.error
        { message = "Import of core module found : " ++ name
        , details =
            [ "The import of a core module is not necessary, because they are imported by default"
            , "For a list of all default imports take a look at https://package.elm-lang.org/packages/elm/core/latest/"
            ]
        }
        (Node.range node)
    ]

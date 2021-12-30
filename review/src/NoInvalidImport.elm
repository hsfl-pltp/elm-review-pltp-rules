module NoInvalidImport exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node exposing (Node(..))
import Import.NoCoreModule as NoCoreModule
import Import.NoUnqualified as NoUnqualified exposing (importVisitor)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports... REPLACEME

    config =
        [ NoInvalidImport.rule
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
elm-review --template undefined/example --rules NoInvalidImport
```

-}
type alias Config =
    List String


type alias Visitor =
    Node Import -> List (Error {})


importVisitors : Config -> List Visitor
importVisitors config =
    [ NoCoreModule.importVisitor
    , NoUnqualified.importVisitor config
    ]


rule : Config -> Rule
rule config =
    Rule.newModuleRuleSchema "NoInvalidImport" ()
        |> Rule.withSimpleImportVisitor (importVisitor config)
        |> Rule.fromModuleRuleSchema


importVisitor : Config -> Node Import -> List (Error {})
importVisitor config node =
    first node (importVisitors config)


first : a -> List (a -> List b) -> List b
first a list =
    case list of
        [] ->
            []

        x :: xs ->
            let
                result =
                    x a
            in
            case result of
                [] ->
                    first a xs

                _ ->
                    result

module NoUnqualifiedImports exposing (rule)

{-| Forbids the use of unqualified imports, expect of a white list.

@docs rule

-}

import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Expression exposing (Function)
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of unqualified imports, with a whitelist of modules, which are allowed to import unqualified, like Html or Svg.

    config =
        [ OnlyQualifiedImports.rule [ "Html", "Svg" ]
        ]


## Fail

    import Foo exposing (bar)

    a =
        bar 17


## Success

    import Foo

    a =
        Foo.bar 17

-}
rule : List String -> Rule
rule whiteList =
    Rule.newModuleRuleSchema "OnlyQualifiedImports" ()
        |> Rule.withSimpleImportVisitor (\node -> importVisitor whiteList node)
        |> Rule.fromModuleRuleSchema


isWhiteListModule : String -> List String -> Bool
isWhiteListModule name whiteList =
    List.member name whiteList


moduleName : Import -> String
moduleName i =
    String.join "." (Node.value i.moduleName)


exposingFromImport : Import -> Maybe Exposing
exposingFromImport { exposingList } =
    Maybe.map Node.value exposingList


importVisitor : List String -> Node Import -> List (Error {})
importVisitor whiteList node =
    if isWhiteListModule (moduleName (Node.value node)) whiteList then
        []

    else
        case exposingFromImport (Node.value node) of
            Nothing ->
                []

            Just exp ->
                checkExposed node exp


checkExposed : Node Import -> Exposing -> List (Error {})
checkExposed node e =
    case e of
        All _ ->
            exposingAllError node

        Explicit list ->
            validateExposingList list


validateExposingList : List (Node TopLevelExpose) -> List (Error {})
validateExposingList list =
    case list of
        [] ->
            []

        x :: xs ->
            validateExpose x ++ validateExposingList xs


validateExpose : Node TopLevelExpose -> List (Error {})
validateExpose node =
    case Node.value node of
        FunctionExpose func ->
            ruleErrors node func

        _ ->
            []


exposingAllError : Node Import -> List (Error {})
exposingAllError node =
    [ Rule.error
        { message = "Importing everything from a Module is not an qualified import."
        , details =
            [ "Only import the things you need, for example Types"
            , "A qualified import is something like 'import Foo'"
            , "and should be used like 'Foo.bar', to make clear, from which module 'bar' is coming from"
            ]
        }
        (Node.range node)
    ]


ruleErrors : Node TopLevelExpose -> String -> List (Error {})
ruleErrors node func =
    [ Rule.error
        { message = "This is not an qualified import: " ++ func
        , details =
            [ "A qualified import is a import, only exposing Types, like  \"import Foo exposing (MyCustomType)\""
            , "This make it easier to determine from which module the function is coming from"
            ]
        }
        (Node.range node)
    ]

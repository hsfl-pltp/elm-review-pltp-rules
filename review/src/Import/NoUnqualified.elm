module Import.NoUnqualified exposing (rule, importVisitor)

{-| Forbids the use of unqualified imports, expect of a white list.

@docs rule

-}

import Elm.Syntax.Exposing exposing (Exposing(..), TopLevelExpose(..))
import Elm.Syntax.Import exposing (Import)
import Elm.Syntax.Node as Node exposing (Node(..))
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of unqualified imports, with a whitelist of modules, which are allowed to import unqualified, like Html or Svg.

    config =
        [ OnlyQualifiedImports.rule [ "Html", "Html.Attributes" ]
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
    Rule.newModuleRuleSchema "NoUnqualifiedImports" ()
        |> Rule.withSimpleImportVisitor (importVisitor whiteList)
        |> Rule.fromModuleRuleSchema


importVisitor : List String -> Node Import -> List (Error {})
importVisitor whiteList (Node _ importValue) =
    if List.member (toModuleName importValue) whiteList then
        []

    else
        errorsForImport importValue


toModuleName : Import -> String
toModuleName i =
    String.join "." (Node.value i.moduleName)


errorsForImport : Import -> List (Error {})
errorsForImport imp =
    case exposingFromImport imp of
        Nothing ->
            []

        Just exp ->
            errorsForExposing exp


exposingFromImport : Import -> Maybe Exposing
exposingFromImport { exposingList } =
    Maybe.map Node.value exposingList


errorsForExposing : Exposing -> List (Error {})
errorsForExposing exp =
    case exp of
        Explicit list ->
            List.concatMap errorsForTopLevelExpose list

        _ ->
            []


errorsForTopLevelExpose : Node TopLevelExpose -> List (Error {})
errorsForTopLevelExpose node =
    case Node.value node of
        FunctionExpose func ->
            [ exposeError node func ]

        _ ->
            []


exposeError : Node TopLevelExpose -> String -> Error {}
exposeError node func =
    Rule.error
        { message = "This is not an qualified import: " ++ func
        , details =
            [ "A qualified import is a import, only exposing Types, like  \"import Foo exposing (MyCustomType)\""
            , "This make it easier to determine from which module the function is coming from."
            ]
        }
        (Node.range node)

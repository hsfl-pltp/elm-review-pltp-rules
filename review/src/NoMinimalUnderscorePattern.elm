module NoMinimalUnderscorePattern exposing (rule)

{-| Forbids the Use of the underscore pattern, if less patterns covered by the underscore pattern then a defined threshold

@docs rule

-}

import Dict exposing (Dict)
import Elm.Syntax.Declaration as Declaration exposing (Declaration(..))
import Elm.Syntax.Expression as Expression exposing (Expression(..))
import Elm.Syntax.ModuleName exposing (ModuleName)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import List.Extra
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)
import Set exposing (Set)


{-| Reports the use of the underscore pattern, with less then 5 patterns covered.

    config =
        [ NoMinimalUnderscorePattern.rule 5
        ]


## Fail

    type MyCustomType
        = One
        | Two
        | Three

    case myCustomType of
        One ->
            ...
        _ ->
            ...


## Success

    case myCustomType of
        One ->
            ...
        Two  ->
            ...
        Three ->
            ...

-}
rule : Int -> Rule
rule threshold =
    Rule.newProjectRuleSchema "NoMinimalUnderscorePattern" (initProjectContext threshold)
        |> Rule.withModuleVisitor moduleVisitor
        |> Rule.withModuleContextUsingContextCreator
            { fromProjectToModule = fromProjectToModule
            , fromModuleToProject = fromModuleToProject
            , foldProjectContexts = foldProjectContexts
            }
        |> Rule.withContextFromImportedModules
        |> Rule.fromProjectRuleSchema



-- Module Visitor


moduleVisitor : Rule.ModuleRuleSchema schemaState ModuleContext -> Rule.ModuleRuleSchema { schemaState | hasAtLeastOneVisitor : () } ModuleContext
moduleVisitor schema =
    schema
        |> Rule.withDeclarationListVisitor declarationListVisitor
        |> Rule.withExpressionEnterVisitor expressionEnterVisitor



--  Declaration List Visitor


declarationListVisitor : List (Node Declaration) -> ModuleContext -> ( List nothing, ModuleContext )
declarationListVisitor declarations context =
    ( [], { context | customTypes = Dict.insert [] (getCustomTypes declarations) context.customTypes } )


getCustomTypes : List (Node Declaration) -> Dict String (Set String)
getCustomTypes declarations =
    declarations
        |> List.filterMap customType
        |> List.map (\type_ -> ( Node.value type_.name, typeConstructors type_.constructors ))
        |> Dict.fromList


customType : Node Declaration -> Maybe Type
customType node =
    case Node.value node of
        Declaration.CustomTypeDeclaration type_ ->
            Just type_

        _ ->
            Nothing


typeConstructors : List (Node ValueConstructor) -> Set String
typeConstructors list =
    list
        |> List.map valueConstructorName
        |> Set.fromList


valueConstructorName : Node ValueConstructor -> String
valueConstructorName (Node _ { name }) =
    Node.value name



-- expressionEnterVisitor


expressionEnterVisitor : Node Expression -> ModuleContext -> ( List (Error {}), ModuleContext )
expressionEnterVisitor node context =
    case Node.value node of
        Expression.CaseExpression { cases } ->
            case findAllPattern cases of
                Nothing ->
                    ( [], context )

                Just ( pattern, _ ) ->
                    ( checkCase pattern cases context, context )

        _ ->
            ( [], context )


checkCase : Node Pattern -> List Expression.Case -> ModuleContext -> List (Error {})
checkCase node cases { customTypes, threshold } =
    case List.head cases of
        Nothing ->
            []

        Just ( pattern, _ ) ->
            case Node.value pattern of
                Pattern.NamedPattern { moduleName, name } _ ->
                    let
                        all =
                            findAllConstructors customTypes moduleName name

                        used =
                            findUsedConstructors cases
                    in
                    if inThreshold threshold used all then
                        ruleErrors threshold node used all

                    else
                        []

                _ ->
                    []


findAllPattern : List Expression.Case -> Maybe ( Node Pattern, Node Expression )
findAllPattern cases =
    List.Extra.find (\( pattern, _ ) -> isAllPattern pattern) cases


isAllPattern : Node Pattern -> Bool
isAllPattern pattern =
    case Node.value pattern of
        Pattern.AllPattern ->
            True

        _ ->
            False


findUsedConstructors : List Expression.Case -> List String
findUsedConstructors cases =
    case cases of
        [] ->
            []

        ( pattern, _ ) :: xs ->
            case Node.value pattern of
                Pattern.NamedPattern { name } _ ->
                    name :: findUsedConstructors xs

                _ ->
                    findUsedConstructors xs


findAllConstructors : CustomTypes -> ModuleName -> String -> List String
findAllConstructors customTypes moduleName constructor =
    case Dict.get moduleName customTypes of
        Nothing ->
            []

        Just t ->
            Maybe.withDefault [] (findTypeByConstructor t constructor)


findTypeByConstructor : Dict String (Set String) -> String -> Maybe (List String)
findTypeByConstructor customTypes constructor =
    customTypes
        |> Dict.toList
        |> List.map Tuple.second
        |> List.Extra.find (\k -> List.member constructor (Set.toList k))
        |> Maybe.map Set.toList


inThreshold : Int -> List String -> List String -> Bool
inThreshold threshold used all =
    List.length all - threshold >= List.length used



-- Rule Error


ruleErrors : Int -> Node a -> List String -> List String -> List (Error {})
ruleErrors threshold node used all =
    [ Rule.error
        { message = "To less covered cases by the underscore pattern, at lest " ++ String.fromInt threshold ++ " cases should be covered!"
        , details =
            [ "The underscore pattern should be used with care."
            , "You only covered "
                ++ String.fromInt (List.length used)
                ++ " of "
                ++ String.fromInt (List.length all)
                ++ " cases!"
            , "When you extend your custom type, then the new constructor will not be covered in the case expression."
            , "I suggest, when your custom type has less then " ++ String.fromInt threshold ++ " constructors, you should not use the underscore pattern"
            ]
        }
        (Node.range node)
    ]



-- Context


type alias CustomTypes =
    Dict ModuleName (Dict String (Set String))


type alias ProjectContext =
    { customTypes : CustomTypes
    , threshold : Int
    }


type alias ModuleContext =
    { lookupTable : ModuleNameLookupTable
    , customTypes : CustomTypes
    , threshold : Int
    }


initProjectContext : Int -> ProjectContext
initProjectContext threshold =
    { customTypes = Dict.empty
    , threshold = threshold
    }


fromProjectToModule : Rule.ContextCreator ProjectContext ModuleContext
fromProjectToModule =
    Rule.initContextCreator
        (\lookupTable projectContext ->
            { lookupTable = lookupTable
            , customTypes = projectContext.customTypes
            , threshold = projectContext.threshold
            }
        )
        |> Rule.withModuleNameLookupTable


fromModuleToProject : Rule.ContextCreator ModuleContext ProjectContext
fromModuleToProject =
    Rule.initContextCreator
        (\metadata moduleContext ->
            { customTypes = projectCustomTypes moduleContext.customTypes metadata
            , threshold = moduleContext.threshold
            }
        )
        |> Rule.withMetadata


projectCustomTypes : Dict ModuleName (Dict String (Set String)) -> Rule.Metadata -> Dict ModuleName (Dict String (Set String))
projectCustomTypes c metadata =
    c
        |> Dict.get []
        |> Maybe.withDefault Dict.empty
        |> Dict.singleton (Rule.moduleNameFromMetadata metadata)


foldProjectContexts : ProjectContext -> ProjectContext -> ProjectContext
foldProjectContexts newContext prevContext =
    { customTypes = Dict.union newContext.customTypes prevContext.customTypes
    , threshold = newContext.threshold
    }

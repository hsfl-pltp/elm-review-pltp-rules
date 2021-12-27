module NoForbiddenFeatures exposing
    ( rule
    , Config
    )

{-| Forbids the use of functions and features, defined by configuration

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Type exposing (Type, ValueConstructor)
import Helpers
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of forbidden features and functions

    config =
        [ NoForbiddenFeatures.rule
            { operators = [ "|>", "<|" ]
            , functions = [ "List.map", "List.foldr" ]
            , letIn = True
            , productDataTypes = False
            }
        ]


## Example for operators


### Fail

    foo bar =
        bar |> func


### Success

    foo bar =
        func bar


## Example for functions

    foo bar =
        List.map func bar

    foo bar =
        case bar of
            ...
            ...

-}
type alias Config =
    { operators : List String
    , functions : List String
    , letIn : Bool
    , productDataTypes : Bool
    , lambda : Bool
    }



-- Context


type alias Context =
    { lookupTable : ModuleNameLookupTable
    , config : Config
    }


initialContext : Config -> Rule.ContextCreator () Context
initialContext config =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable
            , config = config
            }
        )
        |> Rule.withModuleNameLookupTable


rule : Config -> Rule
rule config =
    Rule.newModuleRuleSchemaUsingContextCreator "NoForbiddenFeatures" (initialContext config)
        |> Rule.withDeclarationEnterVisitor declarationVisitor
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema



-- Declaration visitor


declarationVisitor : Node Declaration -> Context -> ( List (Error {}), Context )
declarationVisitor node context =
    if context.config.productDataTypes then
        case Node.value node of
            Declaration.CustomTypeDeclaration nodeType ->
                ( validateCustomTypeDeclaration nodeType, context )

            _ ->
                ( [], context )

    else
        ( [], context )


validateCustomTypeDeclaration : Type -> List (Error {})
validateCustomTypeDeclaration { constructors } =
    List.concatMap validateValueConstructor constructors


validateValueConstructor : Node ValueConstructor -> List (Error {})
validateValueConstructor (Node range valueConstructor) =
    if List.isEmpty valueConstructor.arguments then
        []

    else
        [ ruleErrors "product data types" (Node range valueConstructor) ]



-- Expression visitor


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.OperatorApplication operator _ _ _ ->
            ( validateFeature context.config.operators operator node, context )

        Expression.FunctionOrValue _ func ->
            ( validateFeature context.config.functions (Helpers.functionName node context.lookupTable func) node, context )

        Expression.LetExpression _ ->
            ( validateLetIn context.config.letIn node, context )

        Expression.LambdaExpression _ ->
            ( validate context.config.lambda (ruleErrors "lambda expressions") node, context )

        _ ->
            ( [], context )


validate : Bool -> (Node a -> Error {}) -> Node a -> List (Error {})
validate isEnabled toError node =
    if isEnabled then
        [ toError node ]

    else
        []


validateFeature : List String -> String -> Node Expression -> List (Error {})
validateFeature forbidden feature node =
    validate (List.member feature forbidden) (ruleErrors feature) node


validateLetIn : Bool -> Node Expression -> List (Error {})
validateLetIn enabled =
    validate enabled (ruleErrors "let .. in ..")


ruleErrors : String -> Node a -> Error {}
ruleErrors feature node =
    Rule.error
        { message = "The use of " ++ feature ++ " is forbidden!"
        , details =
            [ "You have to solve the problem in another way..."
            ]
        }
        (Node.range node)

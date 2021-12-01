module UseEtaReductions exposing (rule)

{-| Forbids function declarations without eta reductions

@docs rule

-}

import Elm.Syntax.Declaration as Declaration exposing (Declaration)
import Elm.Syntax.Expression as Expression exposing (Expression, Function, FunctionImplementation)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern as Pattern exposing (Pattern)
import List.Extra
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the possibility to apply a eta reduction

    config =
        [ UseEtaReductions.rule
        ]


## Fail

    incList : List Int -> List Int
    incList list =
        map inc list


## Success

    incList : List Int -> List Int
    incList =
        map inc

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "UseEtaReductions" ()
        |> Rule.withSimpleDeclarationVisitor declarationVisitor
        |> Rule.fromModuleRuleSchema


declarationVisitor : Node Declaration -> List (Error {})
declarationVisitor node =
    case Node.value node of
        Declaration.FunctionDeclaration fn ->
            validateFunction fn

        _ ->
            []


validateFunction : Function -> List (Error {})
validateFunction { declaration } =
    validateFunctionImplementation declaration


validateFunctionImplementation : Node FunctionImplementation -> List (Error {})
validateFunctionImplementation (Node _ { expression, arguments }) =
    case Node.value expression of
        Expression.Application list ->
            validateApplication (List.Extra.last list) (List.Extra.last arguments) applicationErrors

        Expression.OperatorApplication _ _ _ right ->
            validateApplication (Just right) (List.Extra.last arguments) operatorErrors

        _ ->
            []


validateApplication : Maybe (Node Expression) -> Maybe (Node Pattern) -> (Node Expression -> List (Error {})) -> List (Error {})
validateApplication expression argument error =
    case expression of
        Nothing ->
            []

        Just e ->
            case argument of
                Nothing ->
                    []

                Just pattern ->
                    if isEqualPattern e pattern then
                        error e

                    else
                        []


isEqualPattern : Node Expression -> Node Pattern -> Bool
isEqualPattern expression pattern =
    case Node.value pattern of
        Pattern.VarPattern var ->
            case Node.value expression of
                Expression.FunctionOrValue [] val ->
                    var == val

                _ ->
                    False

        _ ->
            False


applicationErrors : Node Expression -> List (Error {})
applicationErrors node =
    [ Rule.error
        { message = "Possible eta reduction detected."
        , details =
            [ "When the last argument of a function is the last applied to your expression, then you should remove both"
            , "Imagine you have a function with the signature incList : List Int -> List Int, with the implementation \"incList list = List.map inc list\""
            , "When you apply the eta reduction, you can remove the list argument and the last argument of the List.map function : \" incList = List.map inc\""
            ]
        }
        (Node.range node)
    ]


operatorErrors : Node Expression -> List (Error {})
operatorErrors node =
    [ Rule.error
        { message = "Possible eta reduction for operator detected."
        , details =
            [ "When the last argument of a function is the last applied to your expression then you should remove both"
            , "This operator can be written as infix, for example \" addOne = (+) 1 \""
            ]
        }
        (Node.range node)
    ]

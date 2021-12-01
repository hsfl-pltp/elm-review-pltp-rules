module UseCommutingConversions exposing (rule)

{-| Forbids the use of commuting conversions in if and case expressions

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node(..))
import Elm.Syntax.Pattern exposing (Pattern)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports the use of commuting conversions in if and case expressions

    config =
        [ UseCommutingConversionsIf.rule
        ]


## Fail

    a =
        if a then
            f b

        else
            f c


## Success

    a =
        f
            (if a then
                b

             else
                c
            )

-}
rule : Rule
rule =
    Rule.newModuleRuleSchema "UseCommutingConversionsIf" ()
        -- Add your visitors
        |> Rule.withSimpleExpressionVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> List (Error {})
expressionVisitor node =
    case Node.value node of
        Expression.IfBlock _ left right ->
            isCommutingConversion node ifErrors [ left, right ]

        Expression.CaseExpression { cases } ->
            isCommutingConversionCase node cases

        _ ->
            []


isCommutingConversionCase : Node Expression -> List ( Node Pattern, Node Expression ) -> List (Error {})
isCommutingConversionCase node list =
    list
        |> List.map Tuple.second
        |> isCommutingConversion node caseErrors


isCommutingConversion : Node Expression -> (Node Expression -> List (Error {})) -> List (Node Expression) -> List (Error {})
isCommutingConversion node error nodes =
    case nodes of
        [] ->
            []

        x :: xs ->
            case Node.value x of
                Expression.Application applications ->
                    case getFirstApplication applications of
                        Nothing ->
                            []

                        Just func ->
                            isCommutingConversionHelper node error xs func

                _ ->
                    []


isCommutingConversionHelper : Node Expression -> (Node Expression -> List (Error {})) -> List (Node Expression) -> String -> List (Error {})
isCommutingConversionHelper node error nodes acc =
    case nodes of
        [] ->
            error node

        x :: xs ->
            if isSameAsPrevious x acc then
                isCommutingConversionHelper node error xs acc

            else
                []


isSameAsPrevious : Node Expression -> String -> Bool
isSameAsPrevious node previous =
    case Node.value node of
        Expression.Application a ->
            case getFirstApplication a of
                Nothing ->
                    False

                Just current ->
                    current == previous

        _ ->
            False


getFirstApplication : List (Node Expression) -> Maybe String
getFirstApplication list =
    list
        |> List.head
        |> Maybe.andThen isFunctionOrValue


isFunctionOrValue : Node Expression -> Maybe String
isFunctionOrValue node =
    case Node.value node of
        Expression.FunctionOrValue _ f ->
            Just f

        _ ->
            Nothing


ifErrors : Node Expression -> List (Error {})
ifErrors node =
    [ Rule.error
        { message = "Possible commuting conversion for if detected"
        , details =
            [ "An expression in the form \"if a then f b else f c\" should be written as \"f(if a then b else c)\""
            ]
        }
        (Node.range node)
    ]


caseErrors : Node Expression -> List (Error {})
caseErrors node =
    [ Rule.error
        { message = "Possible commuting conversion for case detected"
        , details =
            [ "An expression in the form \"case e of p1 -> f e1 ... pn -> f en\" should be written as \"f (case e of p1 -> e1 ... pn -> en)\""
            ]
        }
        (Node.range node)
    ]

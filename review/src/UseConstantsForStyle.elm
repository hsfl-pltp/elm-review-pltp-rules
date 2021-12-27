module UseConstantsForStyle exposing (rule)

{-|

@docs rule

-}

import Elm.Syntax.Expression as Expression exposing (Expression)
import Elm.Syntax.Node as Node exposing (Node)
import Helpers
import List.Extra
import Review.ModuleNameLookupTable exposing (ModuleNameLookupTable)
import Review.Rule as Rule exposing (Error, Rule)


{-| Reports... REPLACEME

    config =
        [ UseConstantsForStyle.rule
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
elm-review --template undefined/example --rules UseConstantsForStyle
```

-}



-- Context


htmlElements : List String
htmlElements =
    [ "text"
    , "node"
    , "map"
    , "h1"
    , "h2"
    , "h3"
    , "h4"
    , "h5"
    , "h6"
    , "div"
    , "p"
    , "hr"
    , "pre"
    , "blockquote"
    , "span"
    , "a"
    , "code"
    , "em"
    , "strong"
    , "i"
    , "b"
    , "u"
    , "sub"
    , "sup"
    , "br"
    , "ol"
    , "ul"
    , "li"
    , "dl"
    , "dt"
    , "dd"
    , "img"
    , "iframe"
    , "canvas"
    , "math"
    , "form"
    , "input"
    , "textarea"
    , "button"
    , "select"
    , "option"
    , "section"
    , "nav"
    , "article"
    , "aside"
    , "header"
    , "footer"
    , "address"
    , "main_"
    , "figure"
    , "figcaption"
    , "table"
    , "caption"
    , "colgroup"
    , "col"
    , "tbody"
    , "thead"
    , "tfoot"
    , "tr"
    , "td"
    , "th"
    , "fieldset"
    , "legend"
    , "label"
    , "datalist"
    , "optgroup"
    , "output"
    , "progress"
    , "meter"
    , "audio"
    , "video"
    , "source"
    , "track"
    , "embed"
    , "object"
    , "param"
    , "ins"
    , "del"
    , "small"
    , "cite"
    , "dfn"
    , "abbr"
    , "time"
    , "var"
    , "samp"
    , "kbd"
    , "s"
    , "q"
    , "mark"
    , "ruby"
    , "rt"
    , "rp"
    , "bdi"
    , "bdo"
    , "wbr"
    , "details"
    , "summary"
    , "menuitem"
    , "menu"
    ]


type alias Context =
    { lookupTable : ModuleNameLookupTable }


initialContext : Rule.ContextCreator () Context
initialContext =
    Rule.initContextCreator
        (\lookupTable () ->
            { lookupTable = lookupTable }
        )
        |> Rule.withModuleNameLookupTable


rule : Rule
rule =
    Rule.newModuleRuleSchemaUsingContextCreator "UseConstantsForStyle" initialContext
        |> Rule.withExpressionEnterVisitor expressionVisitor
        |> Rule.fromModuleRuleSchema


expressionVisitor : Node Expression -> Context -> ( List (Error {}), Context )
expressionVisitor node context =
    case Node.value node of
        Expression.Application (element :: attributes :: _) ->
            ( validateHtmlElement element attributes context, context )

        _ ->
            ( [], context )


validateHtmlElement : Node Expression -> Node Expression -> Context -> List (Error {})
validateHtmlElement node attributes context =
    if isHtmlElement node context.lookupTable then
        validateAttributes attributes context

    else
        []


validateAttributes : Node Expression -> Context -> List (Error {})
validateAttributes node context =
    case Node.value node of
        Expression.ListExpr nodes ->
            if List.Extra.some (isStyleAttribute context.lookupTable) nodes then
                [ ruleError node ]

            else
                []

        -- if List.Extra.some (isStyleAttribute context.lookupTable) attributes then
        -- else
        -- []
        _ ->
            []


isHtmlElement : Node Expression -> ModuleNameLookupTable -> Bool
isHtmlElement node lookupTable =
    case Node.value node of
        Expression.FunctionOrValue _ func ->
            let
                qualifiedName =
                    Helpers.functionName node lookupTable func
            in
            isFromHtmlModule qualifiedName && List.member func htmlElements

        _ ->
            False


isStyleAttribute : ModuleNameLookupTable -> Node Expression -> Bool
isStyleAttribute lookupTable node =
    case Node.value node of
        Expression.FunctionOrValue _ func ->
            func == "style"

        Expression.Application (x :: _) ->
            isStyleAttribute lookupTable x

        _ ->
            False


isFromHtmlModule : String -> Bool
isFromHtmlModule =
    String.startsWith "Html"


ruleError : Node Expression -> Error {}
ruleError node =
    Rule.error
        { message = "Group styles into a constant"
        , details =
            [ "To structure your code, you should use a constant for styles"
            , "Move all attributes for style, into a constant. For Example \"myStyle = [ style ..., style...]\""
            ]
        }
        (Node.range node)

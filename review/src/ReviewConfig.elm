module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoCoreModuleImports
import NoIfNegations
import NoMinimalUnderscorePattern
import OnlyQualifiedImports
import Review.Rule exposing (Rule)
import UnnecessaryIf


config : List Rule
config =
    [ NoCoreModuleImports.rule
    , OnlyQualifiedImports.rule
        [ "Html"
        , "Html.Attributes"
        , "Html.Events"
        , "Svg"
        ]
    , UnnecessaryIf.rule
    , NoMinimalUnderscorePattern.rule 4
    , NoIfNegations.rule
    ]

module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoCoreModuleImports
import OnlyQualifiedImports
import UnnecessaryIf
import Review.Rule exposing (Rule)


config : List Rule
config =
    [ NoCoreModuleImports.rule
    , OnlyQualifiedImports.rule [ "Html", "Svg" ]
    , UnnecessaryIf.rule
    ]
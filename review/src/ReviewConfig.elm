module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoCoreModuleImports
import NoForbiddenFeatures
import NoIfNegations
import NoMinimalRecordDestructing
import NoMinimalUnderscorePattern
import NoOnlyUnQualifiedImports
import Review.Rule exposing (Rule)
import NoUnnecessaryIf
import UseCommutingConversions
import UseEtaReductions


config : List Rule
config =
    [ NoCoreModuleImports.rule
    , NoOnlyUnQualifiedImports.rule
        [ "Html"
        , "Html.Attributes"
        , "Html.Events"
        , "Svg"
        ]
    , NoUnnecessaryIf.rule
    , NoMinimalUnderscorePattern.rule 4
    , NoMinimalRecordDestructing.rule 1
    , NoForbiddenFeatures.rule
        { operators = [ "|>" ]
        , functions = [ "List.map" ]
        }
    , NoIfNegations.rule
    , UseEtaReductions.rule
    , UseCommutingConversions.rule
    ]

module ReviewConfig exposing (config)

{-| Do not rename the ReviewConfig module or the config function, because
`elm-review` will look for these.

To add packages that contain rules, add them to this review project using

    `elm install author/packagename`

when inside the directory containing this file.

-}

import NoCoreModuleImports
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoForbiddenFeatures
import NoIfNegations
import NoImportingEverything
import NoMinimalRecordDestructing
import NoMinimalUnderscorePattern
import NoMissingTypeAnnotation
import NoUnnecessaryIf
import NoUnqualifiedImports
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule exposing (Rule)
import UseCamelCase
import UseCommutingConversions
import UseEtaReductions
import UseLogicalOperators


config : List Rule
config =
    [ NoCoreModuleImports.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoExposingEverything.rule
    , NoForbiddenFeatures.rule
        { operators = [ "|>" ]
        , functions = [ "List.map" ]
        }
    , NoIfNegations.rule
    , NoImportingEverything.rule []
    , NoMinimalRecordDestructing.rule 1
    , NoMinimalUnderscorePattern.rule 4
    , NoMissingTypeAnnotation.rule
    , NoUnnecessaryIf.rule
    , NoUnqualifiedImports.rule
        [ "Html"
        , "Html.Attributes"
        , "Html.Events"
        , "Svg"
        ]
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , UseCommutingConversions.rule
    , UseCamelCase.rule UseCamelCase.default
    , UseEtaReductions.rule
    , UseLogicalOperators.rule
    ]

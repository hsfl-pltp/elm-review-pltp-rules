module ReviewConfig exposing (config)

{-| This file configures elm-review

Please do not change anything here

-}

import NoCoreModuleImports
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoForbiddenFeatures
import NoIfCascade
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
    , NoIfCascade.rule
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

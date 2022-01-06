module ReviewConfig exposing (config)

{-| This file configures elm-review

Please do not change anything here!

-}

import NoBooleanComparison
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoForbiddenFeatures
import NoIfCascade
import NoIfNegations
import NoImportingEverything
import NoInvalidImport
import NoMinimalRecordAccess
import NoMinimalUnderscorePattern
import NoMissingTypeAnnotation
import NoUnnecessaryIf
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
import UseConstantsForStyle
import UseEtaReductions
import UseLogicalOperators


config : List Rule
config =
    [ NoBooleanComparison.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoExposingEverything.rule
    , NoForbiddenFeatures.rule
        { operators = [ "|>" ]
        , functions = [ "List.map", "Html.Attributes.class" ]
        , letIn = True
        , algebraicDataTypes = True
        , lambda = True
        }
    , NoIfCascade.rule
    , NoIfNegations.rule
    , NoImportingEverything.rule []
    , NoMinimalRecordAccess.rule 2
    , NoMinimalUnderscorePattern.rule 4
    , NoMissingTypeAnnotation.rule
    , NoUnnecessaryIf.rule
    , NoInvalidImport.rule
        [ "Html"
        , "Html.Attributes"
        , "Html.Events"
        , "Svg"
        , "Svg.Attributes"
        ]
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , UseCommutingConversions.rule
    , UseConstantsForStyle.rule
    , UseCamelCase.rule UseCamelCase.default
    , UseEtaReductions.rule
    , UseLogicalOperators.rule
    ]

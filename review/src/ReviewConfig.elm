module ReviewConfig exposing (config)

import NoAlways
import NoBooleanCase
import NoDebug.Log
import NoDebug.TodoOrToString
import NoExposingEverything
import NoForbiddenWords
import NoImportingEverything
import NoLeftPizza
import NoMissingTypeAnnotation
import NoRedundantConcat
import NoRedundantCons
import NoUnused.CustomTypeConstructors
import NoUnused.Dependencies
import NoUnused.Exports
import NoUnused.Modules
import NoUnused.Parameters
import NoUnused.Patterns
import NoUnused.Variables
import Review.Rule as Rule exposing (Rule)
import UseCamelCase
import Vendor.NoFullyAppliedPrefixOperator as NoFullyAppliedPrefixOperator


config : List Rule
config =
    [ NoAlways.rule
    , NoBooleanCase.rule
    , NoDebug.Log.rule
    , NoDebug.TodoOrToString.rule
    , NoExposingEverything.rule
    , NoForbiddenWords.rule
        [ "- [ ]"
        , "TODO"
        ]
    , NoFullyAppliedPrefixOperator.rule
    , NoImportingEverything.rule []
    , NoLeftPizza.rule
        |> Rule.ignoreErrorsForDirectories
            [ -- Test functions are traditionally built up using a left pizza.
              -- While we don't want them in our regular code, let's allow them
              -- just for tests.
              "tests/"
            ]
    , NoMissingTypeAnnotation.rule
    , NoRedundantConcat.rule
    , NoRedundantCons.rule
    , NoUnused.CustomTypeConstructors.rule []
    , NoUnused.Dependencies.rule
    , NoUnused.Exports.rule
    , NoUnused.Modules.rule
    , NoUnused.Parameters.rule
    , NoUnused.Patterns.rule
    , NoUnused.Variables.rule
    , UseCamelCase.rule UseCamelCase.default
    ]

module NoInvalidImportTest exposing (all)

import NoInvalidImport exposing (rule)
import Review.Test exposing (ExpectedError)
import Test exposing (Test, describe)
import TestHelper


all : Test
all =
    describe "NoInvalidImport"
        [ testCoreModule "Basics"
        , testCoreModule "List"
        , testCoreModule "Maybe"
        , testCoreModule "Result"
        , testCoreModule "String"
        , testCoreModule "Char"
        , testCoreModule "Tuple"
        , testCoreModule "Debug"
        , testCoreModule "Platform"
        , testCoreModule "Platform.Cmd"
        , testCoreModule "Platform.Sub"
        , testUnqualified "Test exposing (map)" "map"
        , testNoError "Html exposing (div)"
        , testNoError "Html.Attributes exposing (style)"
        , testNoError "Test exposing (Bar, Foo(..))"
        ]


testCoreModule : String -> Test
testCoreModule import_ =
    TestHelper.testRule
        ("Should report an error when the module " ++ import_ ++ " is imported.")
        (source import_)
        (coreError import_)
        (rule [])


testUnqualified : String -> String -> Test
testUnqualified import_ under =
    TestHelper.testRule
        ("Should report an error when the module " ++ import_ ++ " is imported.")
        (source import_)
        (unqualifiedError under)
        (rule ["Html"])

testNoError : String -> Test 
testNoError import_ = 
    TestHelper.testRuleNoErrors
        ("Should not report an error a import like " ++ import_ ++ " is found")
        (source import_)
        (rule ["Html", "Html.Attributes"])

coreError : String -> ExpectedError
coreError import_ =
    Review.Test.error
        { message = "Import of core module found : " ++ import_
        , details =
            [ "The import of a core module is not necessary, because they are imported by default."
            , "For a list of all default imports take a look at https://package.elm-lang.org/packages/elm/core/latest/."
            ]
        , under = "import " ++ import_
        }


unqualifiedError : String -> ExpectedError
unqualifiedError under =
    Review.Test.error
        { message = "This is not an qualified import: " ++ under
        , details =
            [ "A qualified import is a import, only exposing Types, like  \"import Foo exposing (MyCustomType)\""
            , "This make it easier to determine from which module the function is coming from."
            ]
        , under = under
        }


source : String -> String
source import_ =
    """
module A exposing(..)

import """ ++ import_

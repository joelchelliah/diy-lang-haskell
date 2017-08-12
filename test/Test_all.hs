import           Test.Tasty
import           Test_1_Parser
import           Test_2_EvaluatingSimpleExpressions
import           Test_3_EvaluatingComplexExpressions
import           Test_4_WorkingWithVariables
import           Test_5_AddingFunctionsToTheMix
import           Test_6_WorkingWithLists
import           Test_7_UsingTheLanguage

--
-- The following command runs all the tests for all of the parts:
--
-- stack test diy-lang-haskell:test-all
--

main :: IO ()
main = defaultMain
     $ testGroup "\nDIY Lang: All Tests"
       [ parsingTests
       , evaluatingSimpleExpressionsTests
       , evaluatingComplexExpressionsTests
       , workingWithVariabesTests
       , addingFunctionsToTheMixTests
       , workingWithListsTests
       , usingTheLanguageTests
       ]

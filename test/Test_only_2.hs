import           Test.Tasty
import           Test_2_EvaluatingSimpleExpressions

--
-- The following command runs all the tests for part 2:
--
-- stack test diy-lang-haskell:test-2
--

main :: IO ()
main = defaultMain
     $ testGroup "\nDIY Lang: Testing Part 2"
       [ evaluatingSimpleExpressionsTests ]

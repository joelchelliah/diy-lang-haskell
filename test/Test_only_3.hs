import           Test.Tasty
import           Test_3_EvaluatingComplexExpressions

--
-- The following command runs all the tests for part 3:
--
-- stack test diy-lang-haskell:test-3
--

main :: IO ()
main = defaultMain
     $ testGroup "\nDIY Lang: Testing Part 3"
       [ evaluatingComplexExpressionsTests ]

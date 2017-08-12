import           Test.Tasty
import           Test_4_WorkingWithVariables

--
-- The following command runs all the tests for part 4:
--
-- stack test diy-lang-haskell:test-4
--

main :: IO ()
main = defaultMain
     $ testGroup "\nDIY Lang: Testing Part 4"
       [ workingWithVariabesTests ]

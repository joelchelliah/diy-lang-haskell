import           Test.Tasty
import           Test_5_AddingFunctionsToTheMix

--
-- The following command runs all the tests for part 5:
--
-- stack test diy-lang-haskell:test-5
--

main :: IO ()
main = defaultMain
     $ testGroup "\nDIY Lang: Testing Part 5"
       [ addingFunctionsToTheMixTests ]

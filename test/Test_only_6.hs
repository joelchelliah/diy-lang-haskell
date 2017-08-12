import           Test.Tasty
import           Test_6_WorkingWithLists

--
-- The following command runs all the tests for part 6:
--
-- stack test diy-lang-haskell:test-6
--

main :: IO ()
main = defaultMain
     $ testGroup "\nDIY Lang: Testing Part 6"
       [ workingWithListsTests ]

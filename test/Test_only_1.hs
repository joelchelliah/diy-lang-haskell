import           Test.Tasty
import           Test_1_Parser

--
-- The following command runs all the tests for part 1:
--
-- stack test diy-lang-haskell:test-1
--

main :: IO ()
main = defaultMain
     $ testGroup "\nDIY Lang: Testing Part 1"
       [ parsingTests ]

import           Test.Tasty
import           Test_7_UsingTheLanguage

--
-- The following command runs all the tests for part 7:
--
-- stack test diy-lang-haskell:test-7
--

main :: IO ()
main = defaultMain
     $ testGroup "\nDIY Lang: Testing Part 7"
       [ usingTheLanguageTests ]

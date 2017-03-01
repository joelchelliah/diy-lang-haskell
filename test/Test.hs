import           Test.Tasty

import           Test_1_Parsing

main :: IO ()
main = defaultMain tests

tests :: TestTree
tests = testGroup "Tests" [parsingTests]

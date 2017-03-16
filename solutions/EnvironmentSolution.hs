module EnvironmentSolution where

import           Types


-- Solutions for part 4:

lookup' :: Environment -> String -> DiyAST
lookup' env key =
  case env of
    Environment [] -> DiyError $ LookUpError key
    Environment((name,value):rest)
      | name == key -> value
      | otherwise   -> lookup' (Environment rest) key



extend' :: Environment -> (String, DiyAST) -> Environment
extend' (Environment bindings) binding@(name, _) =
  Environment(binding : uniqueBindings)

  where uniqueBindings = filter unique bindings
        unique         = (/= name) . fst

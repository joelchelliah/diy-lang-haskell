module EnvironmentSolution where

import           Types


-- Solutions for part 4:

lookup' :: Environment -> String -> DiyAST
lookup' env key =
  case bindings env of
    [] -> DiyError $ LookUpError key
    ((name,value):rest)
      | name == key -> value
      | otherwise   -> lookup' (Environment rest) key



extend' :: Environment -> (String, DiyAST) -> Environment
extend' env binding@(name, _) =
  Environment(binding : uniqueBindings)

  where uniqueBindings = filter isUnique $ bindings env
        isUnique       = (/= name) . fst

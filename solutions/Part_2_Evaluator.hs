module Part_2_Evaluator where

import           Evaluator hiding (evaluate)
import           Types


-- Solution for part 2: Evaluating simple expressions

evaluate :: DiyAST -> String -> DiyAST
evaluate ast env =
  case ast of
    DiyList list -> evaluateList list
    expression   -> expression

  where evaluateList [DiySymbol "quote", exp] = exp
        evaluateList [DiySymbol "atom", exp]  = isAtom exp
        evaluateList [DiySymbol "eq", e1, e2] = isEqual e1 e2
        evaluateList list                     = DiyList list

        isAtom (DiyList (DiySymbol "quote":_)) = DiyBool True
        isAtom (DiyList _)                     = DiyBool False
        isAtom (DiyError _)                    = DiyBool False
        isAtom _                               = DiyBool True

        isEqual (DiyList [DiySymbol "quote", s1])
                (DiyList [DiySymbol "quote", s2]) = isEqual s1 s2
        isEqual (DiySymbol s1) (DiySymbol s2)     = DiyBool $ s1 == s2
        isEqual (DiyBool s1) (DiyBool s2)         = DiyBool $ s1 == s2
        isEqual (DiyInt s1) (DiyInt s2)           = DiyBool $ s1 == s2
        isEqual _ _                               = DiyBool False

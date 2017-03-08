module EvaluatorSolution where

import           Types


-- Solutions for part 2 - 3: Evaluator

evaluate' :: DiyAST -> String -> DiyAST
evaluate' ast env =
  case ast of
    DiyList list -> evaluateList list
    expression   -> expression

  where eval = flip evaluate' env

        evaluateList [DiySymbol "quote", exp]       = exp
        evaluateList [DiySymbol "atom", exp]        = isAtom exp
        evaluateList [DiySymbol "eq", e1, e2]       = isEqual e1 e2
        evaluateList [DiySymbol "if", cond, e1, e2] = ifElse cond e1 e2
        evaluateList [DiySymbol "+", e1, e2]        = calc plus e1 e2
        evaluateList [DiySymbol "-", e1, e2]        = calc diff e1 e2
        evaluateList [DiySymbol "*", e1, e2]        = calc mult e1 e2
        evaluateList [DiySymbol "/", e1, e2]        = calc divi e1 e2
        evaluateList [DiySymbol "mod", e1, e2]      = calc modu e1 e2
        evaluateList [DiySymbol ">", e1, e2]        = calc gt   e1 e2
        evaluateList list                           = DiyList list


        -- `atom` :

        isAtom (DiyList (DiySymbol "quote":_)) = DiyBool True
        isAtom (DiyList _)                     = DiyBool False
        isAtom (DiyError _)                    = DiyBool False
        isAtom _                               = DiyBool True


        -- `eq` :

        isEqual (DiyList [DiySymbol "quote", s1])
                (DiyList [DiySymbol "quote", s2]) = isEqual s1 s2
        isEqual e1@(DiyList _) e2                 = isEqual (eval e1) e2
        isEqual e1 e2@(DiyList _)                 = isEqual e1 (eval e2)
        isEqual (DiySymbol s1) (DiySymbol s2)     = DiyBool $ s1 == s2
        isEqual (DiyBool s1) (DiyBool s2)         = DiyBool $ s1 == s2
        isEqual (DiyInt s1) (DiyInt s2)           = DiyBool $ s1 == s2
        isEqual _ _                               = DiyBool False


        -- math operators :

        calc op e1@(DiyInt _) e2@(DiyInt _) = e1 `op` e2
        calc op e1@(DiyList _) e2           = calc op (eval e1) e2
        calc op e1 e2@(DiyList _)           = calc op e1 (eval e2)
        calc _ _ _                          = DiyError InvalidArgument

        plus (DiyInt x) (DiyInt y) = DiyInt  $ x + y
        diff (DiyInt x) (DiyInt y) = DiyInt  $ x - y
        mult (DiyInt x) (DiyInt y) = DiyInt  $ x * y
        divi (DiyInt x) (DiyInt y) = DiyInt  $ x `div` y
        modu (DiyInt x) (DiyInt y) = DiyInt  $ x `mod` y
        gt   (DiyInt x) (DiyInt y) = DiyBool $ x > y


        -- `if` :

        ifElse cond e1 e2
          | (eval cond) == (DiyBool True) = eval e1
          | otherwise                     = eval e2

module EvaluatorSolution where

import           Environment
import           Prelude     hiding (lookup)
import           Types


-- Solutions for part 2 - 6:

evaluate' :: DiyAST -> Environment -> (DiyAST, Environment)
evaluate' ast env =
  case ast of
    DiyClosure cFunc cEnv -> evaluateFunction cFunc cEnv
    DiyList    list       -> evaluateList list
    DiySymbol  key        -> (lookup env key, env)
    expression            -> (expression, env)

  where eval exp =
          let (result, _) = evaluate' exp env
          in result


        -- List evaluation :

        evaluateList (DiyClosure cFunc cEnv : args) = evaluateFunctionCall cFunc cEnv args
        evaluateList [DiySymbol "lambda", e1, e2]   = lambda e1 e2
        evaluateList (DiySymbol "lambda" : _)       = (DiyError InvalidArgument, env)
        evaluateList [DiySymbol "define", e1, e2]   = define e1 e2
        evaluateList (DiySymbol "define" : _)       = (DiyError InvalidArgument, env)
        evaluateList [DiySymbol "quote", exp]       = (exp, env)
        evaluateList [DiySymbol "atom", exp]        = (isAtom exp, env)
        evaluateList [DiySymbol "eq", e1, e2]       = (isEqual e1 e2, env)
        evaluateList [DiySymbol "if", cond, e1, e2] = (ifElse cond e1 e2, env)
        evaluateList [DiySymbol "+", e1, e2]        = (calc plus e1 e2, env)
        evaluateList [DiySymbol "-", e1, e2]        = (calc diff e1 e2, env)
        evaluateList [DiySymbol "*", e1, e2]        = (calc mult e1 e2, env)
        evaluateList [DiySymbol "/", e1, e2]        = (calc divi e1 e2, env)
        evaluateList [DiySymbol "mod", e1, e2]      = (calc modu e1 e2, env)
        evaluateList [DiySymbol ">", e1, e2]        = (calc gt   e1 e2, env)
        evaluateList list@(DiySymbol _ : _)         = evaluateListStartingWithSymbol list
        evaluateList list@(DiyList _ : _)           = evaluateListStartingWithList list
        evaluateList other                          = (DiyError NotAFunction, env)


        -- Function evaluation :

        evaluateFunction (DiyFunction fArgs fBody) fEnv =
          evaluate' fBody fEnv

        evaluateFunctionCall func@(DiyFunction fParams fBody) fEnv args =
          evaluate' fBody newEnv

          where newEnv            = foldr (flip extend) fEnv bindings
                bindings          = zip (val <$> fParams) (eval <$> args)
                val (DiySymbol v) = v


        -- Special list evaluations :

        evaluateListStartingWithSymbol (DiySymbol key : rest) =
          evaluate' exp env

          where exp = DiyList $ val : rest
                val = lookup env key

        evaluateListStartingWithList (DiyList list : rest) =
          evaluate' exp env

          where exp      = DiyList $ def : rest
                (def, _) = evaluateList list


        -- `atom` :

        isAtom (DiyList (DiySymbol "quote":_)) = DiyBool True
        isAtom (DiyList _)                     = DiyBool False
        isAtom (DiyError _)                    = DiyBool False
        isAtom _                               = DiyBool True


        -- `eq` :

        isEqual (DiyList [DiySymbol "quote", s1])
                (DiyList [DiySymbol "quote", s2])    = isEqual s1 s2
        isEqual (DiyList l1) (DiyList l2) | l1 == l2 = DiyBool False
        isEqual e1@(DiyList _) e2                    = isEqual (eval e1) e2
        isEqual e1 e2@(DiyList _)                    = isEqual e1 (eval e2)
        isEqual (DiySymbol s1) (DiySymbol s2)        = DiyBool $ s1 == s2
        isEqual (DiyBool s1) (DiyBool s2)            = DiyBool $ s1 == s2
        isEqual (DiyInt s1) (DiyInt s2)              = DiyBool $ s1 == s2
        isEqual _ _                                  = DiyBool False


        -- math operators :

        calc op e1@(DiyInt _) e2@(DiyInt _) = e1 `op` e2
        calc op e1 e2
          | shouldEval e1 = calc op (eval e1) e2
          | shouldEval e2 = calc op e1 (eval e2)
        calc _ _ _                          = DiyError InvalidArgument

        plus (DiyInt x) (DiyInt y) = DiyInt  $ x + y
        diff (DiyInt x) (DiyInt y) = DiyInt  $ x - y
        mult (DiyInt x) (DiyInt y) = DiyInt  $ x * y
        divi (DiyInt x) (DiyInt y) = DiyInt  $ x `div` y
        modu (DiyInt x) (DiyInt y) = DiyInt  $ x `mod` y
        gt   (DiyInt x) (DiyInt y) = DiyBool $ x > y


        -- `if` :

        ifElse cond e1 e2
          | eval cond == DiyBool True = eval e1
          | otherwise                 = eval e2


        -- `define` :

        define (DiySymbol key) exp = (value, newEnv)

          where value  = eval exp
                newEnv = extend env (key, value)

        define _ _ = (DiyError InvalidArgument, env)


        -- `lambda` :

        lambda (DiyList fArgs) fBody =
          (DiyClosure func env, env)

          where func = DiyFunction fArgs fBody

        lambda _ _ = (DiyError InvalidArgument, env)


        -- aux :

        shouldEval (DiyList _)   = True
        shouldEval (DiySymbol _) = True
        shouldEval _             = False

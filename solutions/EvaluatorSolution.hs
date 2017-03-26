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

        evaluateList [DiySymbol sym, e]
          | "quote" == sym = (e, env)
          | shouldEval e   = evaluateList [DiySymbol sym, eval e]
          | "atom"  == sym = (isAtom e, env)
          | "head"  == sym = head' e
          | "tail"  == sym = tail' e
          | "empty" == sym = isEmpty e

        evaluateList [DiySymbol sym, e1, e2]
          | "eq"     == sym     = e1 `eq` e2
          | "define" == sym     = define e1 e2
          | "lambda" == sym     = lambda e1 e2
          | shouldEval e1       = evaluateList [DiySymbol sym, eval e1, e2]
          | shouldEval e2       = evaluateList [DiySymbol sym, e1, eval e2]
          | isMathOperation sym = doMathOperation sym e1 e2
          | "cons" == sym       = cons e1 e2

        evaluateList [DiySymbol "if", cond, e1, e2] = (ifElse cond e1 e2, env)
        evaluateList (DiySymbol "lambda" : _)       = (DiyError InvalidArgument, env)
        evaluateList (DiySymbol "define" : _)       = (DiyError InvalidArgument, env)
        evaluateList (DiyClosure cFunc cEnv : args) = evaluateFunctionCall cFunc cEnv args
        evaluateList list@(DiySymbol _ : _)         = evaluateListStartingWithSymbol list
        evaluateList list@(DiyList _ : _)           = evaluateListStartingWithList list
        evaluateList []                             = (DiyError EmptyFunctionCall, env)
        evaluateList _                              = (DiyError NotAFunction, env)


        -- Function evaluation :

        evaluateFunction (DiyFunction fArgs fBody) =
          evaluate' fBody

        evaluateFunctionCall func@(DiyFunction fParams fBody) fEnv args =
          if numArguments == numParameters
          then evaluate' fBody extEnvWithArgs
          else (DiyError functionArgError, fEnv)

          where extEnvWithArgs    = foldr (flip extend) extendedEnv argBindings
                extendedEnv       = foldr (flip extend) env fEnvBindings
                fEnvBindings      = bindings fEnv
                argBindings       = zip (val <$> fParams) (eval <$> args)
                numParameters     = length fParams
                numArguments      = length args
                val (DiySymbol v) = v
                functionArgError  = InvalidFunctionArguments { expected = numParameters
                                                             , received = numArguments
                                                             }


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
        isAtom _                               = DiyBool True


        -- `eq` :

        e1 `eq` e2 = (isEqual e1 e2, env)

        isEqual e1 e2
          | isQuotedList e1 || isQuotedList e2 = DiyBool False
          | isQuote e1 || isQuote e2           = isEqualQuote e1 e2
          | shouldEval e1                      = isEqual (eval e1) e2
          | shouldEval e2                      = isEqual e1 (eval e2)
        isEqual (DiyBool s1) (DiyBool s2)      = DiyBool $ s1 == s2
        isEqual (DiyInt s1) (DiyInt s2)        = DiyBool $ s1 == s2
        isEqual _ _                            = DiyBool False

        isEqualQuote (DiyList [DiySymbol "quote", s1])
                     (DiyList [DiySymbol "quote", s2])
                     = DiyBool $ s1 == s2


        -- math operations :

        isMathOperation = flip elem ["+",  "-",  "*",  "/",  "mod", ">"]

        doMathOperation "+"   e1 e2 = (calc plus e1 e2, env)
        doMathOperation "-"   e1 e2 = (calc diff e1 e2, env)
        doMathOperation "*"   e1 e2 = (calc mult e1 e2, env)
        doMathOperation "/"   e1 e2 = (calc divi e1 e2, env)
        doMathOperation "mod" e1 e2 = (calc modu e1 e2, env)
        doMathOperation ">"   e1 e2 = (calc gt   e1 e2, env)

        calc op e1@(DiyInt _) e2@(DiyInt _) = e1 `op` e2
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


        -- `cons` :

        cons exp (DiyList list) = (DiyList $ exp : list, env)
        cons exp list           = (DiyError NotAList, env)


        -- `head` :

        head' (DiyList (h:t)) = (h, env)
        head' (DiyList [])    = (DiyError AccessingEmptyList, env)
        head' _               = (DiyError NotAList, env)


        -- `tail` :

        tail' (DiyList (h:t)) = (DiyList t, env)
        tail' (DiyList [])    = (DiyError AccessingEmptyList, env)
        tail' _               = (DiyError NotAList, env)


        -- `empty` :

        isEmpty (DiyList []) = (DiyBool True, env)
        isEmpty (DiyList _ ) = (DiyBool False, env)
        isEmpty _            = (DiyError NotAList, env)


        -- aux :

        shouldEval (DiyList (DiySymbol _ : _)) = True
        shouldEval (DiyList _)                 = False
        shouldEval (DiySymbol _)               = True
        shouldEval _                           = False

        isQuotedList (DiyList [DiySymbol "quote", DiyList _]) = True
        isQuotedList _                                        = False

        isQuote (DiyList [DiySymbol "quote", _]) = True
        isQuote _                                = False

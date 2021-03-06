module Data.Expr.Proposition.Proof where

import           Data.Expr.Proposition.Constr
import           Data.Expr.Proposition.Eval
import           Data.Expr.Proposition.Substitute
import           Data.Expr.Proposition.Types
import           Data.Pretty

import           Data.List                        (intercalate)
import           Data.Maybe                       (listToMaybe)

-- ----------------------------------------

truthTable :: Int -> [[Bool]]
truthTable n
  | n == 0 = [[]]
  | otherwise = map (True :) t' ++ map (False :) t'
    where
  t' = truthTable (n-1)

-- compute a proof by generating a truth table,
-- iterate over all rows in the table
-- and substitute all variable by the values in a row
-- and evaluate the expression
-- if a single result is false
-- we have a counter example, else the expr
-- is a tautology

proof' :: Expr -> Maybe VarEnv
proof' e
 = func environments e
 where
  environments = map (zipWith (\x y -> (x, Lit y)) freeVarList) (truthTable (length (freeVarList)))
  freeVarList = freeVars e
  func [] e = if eval e then Nothing else Just []
  func (env:[]) e = if eval (substVars env e) then Nothing else Just env
  func (env:envs) e = if eval (substVars env e) then func envs e else Just env


proof :: Expr -> String
proof e
  = case proof' e of
     Nothing
       -> pretty e ++ " is a tautology"
     Just env
       -> pretty e ++ " isn't a tautology, " ++
          "a counter example is " ++ ppEnv env
  where
    ppEnv = intercalate ", " . map ppVar
    ppVar (i, v) = i ++ "=" ++ pretty v

-- ----------------------------------------

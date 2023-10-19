module Week41Exercise2 where

import Data.Map (Map)
import qualified Data.Map as Map

data Expr variable
  = Var variable
  | Lit Bool
  | And (Expr variable) (Expr variable)
  | Or (Expr variable) (Expr variable)
  deriving (Eq, Show)

eval :: (Ord variable) => Expr variable -> Map variable Bool -> Maybe Bool
eval (Var x) map = Map.lookup x map
eval (Lit e) _ = Just e
eval (And e1 e2) map = case (eval e1 map, eval e2 map) of
  (Just v1, Just v2) -> Just (v1 && v2)
  othervise -> Nothing
eval (Or e1 e2) map = case (eval e1 map, eval e2 map) of
  (Just v1, Just v2) -> Just (v1 || v2)
  othervise -> Nothing

{-# LANGUAGE TupleSections #-}

module Eval (evaluate) where

import Environment
import Maybe

evaluate :: [Value] -> Env -> Maybe ([Value], Env)
evaluate [] e = Just ([], e)
evaluate (v : vs) e = case eval v e of
	Just (v', e') -> (\(vs', e'') -> (v' : vs', e'')) `mapply` evaluate vs e'
	_ -> Nothing

eval :: Value -> Env -> Maybe (Value, Env)
eval (Symbol s) e = (, e) `mapply` refer s e
eval i@(Int _) e = Just (i, e)
eval l@(List _) e = Just (l, e)

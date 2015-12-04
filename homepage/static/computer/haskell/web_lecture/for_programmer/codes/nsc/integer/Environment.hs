module Environment (Env, M.fromList, Symbol, Value(..), showValue) where

import qualified Data.Map as M

type Env = M.Map Symbol Value

type Symbol = String

data Value =
	Int Integer

showValue :: Value -> String
showValue (Int i) = show i

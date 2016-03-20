module IOMcn (
	IOMcn, runIOMcn, (>>>),
	putHello, putWorld, getLine, getInt, putLine) where

import Prelude hiding (getLine)
import qualified Prelude

newtype IOMcn a b = IOMcn { getIOMcn :: a -> IO b }

runIOMcn :: IOMcn () () -> IO ()
runIOMcn m = getIOMcn m ()

(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
m1 >>> m2 = IOMcn $ \x -> getIOMcn m1 x >>= getIOMcn m2

putHello, putWorld :: IOMcn () ()
putHello = IOMcn . const $ putStrLn "Hello"
putWorld = IOMcn . const $ putStrLn "World"

getLine :: IOMcn () String
getLine = IOMcn $ const Prelude.getLine

getInt :: IOMcn () Int
getInt = IOMcn . const $ fmap read Prelude.getLine

putLine :: IOMcn String ()
putLine = IOMcn putStrLn

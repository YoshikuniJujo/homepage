module StmLock (
	initLock, lock, unlock,
	) where

import Control.Concurrent.STM

data Lock = Locked | Unlocked deriving (Show, Eq)

initLock :: IO (TVar Lock)
initLock = atomically $ newTVar Unlocked

lock :: TVar Lock -> IO ()
lock lv = atomically $ do
	l <- readTVar lv
	check $ l == Unlocked
	writeTVar lv Locked

unlock :: TVar Lock -> IO ()
unlock lv = atomically $ writeTVar lv Unlocked

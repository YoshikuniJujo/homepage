{-# LANGUAGE OverloadedStrings #-}

import Control.Concurrent
import Data.IORef
import System.Random

import qualified Data.ByteString as BS

import StmFile
import StmLock

main :: IO ()
main = do
	update "test.txt" (`BS.append` "HELLO\n")

main_ :: IO ()
main_ = do
	v <- newIORef 23
	l <- initLock
	forkIO $ do
		lock l
		x <- readIORef v
		randomRIO (0, 100000) >>= threadDelay
--		threadDelay 100000
		writeIORef v $ x + 8
		putStrLn "thread end"
		unlock l
	lock l
	randomRIO (0, 100000) >>= threadDelay
	x <- readIORef v
	randomRIO (0, 100000) >>= threadDelay
	writeIORef v $ x + 12
	unlock l
--	threadDelay 100000
	readIORef v >>= print
	threadDelay 500000

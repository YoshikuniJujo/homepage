{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Network

import System.IO
import System.Posix.User

import ShowPage

main :: IO ()
main = do
	soc <- listenOn $ PortNumber 80
	uid <- userID <$> getUserEntryForName "homepage"
	gid <- groupID <$> getGroupEntryForName "homepage"
	setGroupID gid
	setUserID uid
	forever $ do
		(h, x, y) <- accept soc
		void . forkIO $ do
			print h
			print x
			print y
			showPage h
			hClose h

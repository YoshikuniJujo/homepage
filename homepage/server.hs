{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Network

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
		print h
		print x
		print y
		void . forkIO $ showPage h

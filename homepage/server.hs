{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Monad
import Control.Concurrent
import Data.HandleLike
import Data.Pipe
import System.IO
import System.Environment
import Network
import Network.TigHTTP.Server
import Network.TigHTTP.Types

import System.FilePath
import System.Posix.User

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as LBS

import Tools

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
		void . forkIO $ do
			req <- getRequest (DebugHandle h $ Just "low")
			let	Path fp_ = requestPath req
				fp = processIndex $ BSC.unpack fp_
				ex = takeExtension fp
				stp = if ex == ".css" then Css else Html
				tp = ContentType Text stp []
			as <- readFile $ "static/" ++ fp
			let page = if ex == ".html"
				then uncurry (makePage $ BSC.unpack fp_) $
					span (/= '\n') as
				else as
			putResponse h $
				((response :: LBS.ByteString ->
						Response Pipe Handle)
					$ LBS.fromChunks [BSU.fromString page]) {
					responseContentType = tp
					}

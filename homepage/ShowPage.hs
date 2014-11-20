{-# LANGUAGE FlexibleContexts, PackageImports #-}

module ShowPage (showPage) where

import "monads-tf" Control.Monad.Trans
import Data.HandleLike
import Data.Pipe
import System.FilePath
import Network.TigHTTP.Server
import Network.TigHTTP.Types

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as LBS

import Tools

showPage :: (HandleLike h, MonadIO (HandleMonad h)) => h -> HandleMonad h ()
showPage p = do
	req <- getRequest p
	let	Path fp_ = requestPath req
--		fp = if fp_ == "/" then "index.html" else fp_
		fp = processIndex $ BSC.unpack fp_
		ex = takeExtension $ fp
		stp = if ex == ".css" then Css else Html
		tp = ContentType Text stp []
	as <- liftIO . readFile $ "static/" ++ fp
	let	page = if ex == ".html"
			then uncurry (makePage $ BSC.unpack fp_) $ span (/= '\n') as
			else as
	putResponse p $ (responseH p $
		LBS.fromChunks [BSU.fromString page]) { responseContentType = tp }

responseH :: HandleLike h => h -> LBS.ByteString -> Response Pipe h
responseH = const response

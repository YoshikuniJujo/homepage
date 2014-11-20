{-# LANGUAGE FlexibleContexts, PackageImports #-}

module ShowPage (showPage) where

import "monads-tf" Control.Monad.Trans
import Data.HandleLike
import Data.Pipe
import Data.Pipe.List
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
	let	Path fp__ = requestPath req
--		fp = if fp_ == "/" then "index.html" else fp_
		fp_ = takeWhile (/= '?') $ BSC.unpack fp__
		fp = processIndex fp_
		ex = takeExtension $ fp
		stp = if ex == ".css" then Css else Html
		tp = ContentType Text stp []
	liftIO $ print fp__
	getPostData req >>= liftIO . print
	as <- liftIO . readFile $ "static/" ++ fp
	let	page = if ex == ".html"
			then uncurry (makePage fp_) $ span (/= '\n') as
			else as
	putResponse p $ (responseH p $
		LBS.fromChunks [BSU.fromString page]) { responseContentType = tp }

responseH :: HandleLike h => h -> LBS.ByteString -> Response Pipe h
responseH = const response

getPostData :: HandleLike h => Request h -> HandleMonad h (Maybe [BSC.ByteString])
getPostData (RequestPost _ _ Post { postBody = pb }) = runPipe $ pb =$= toList
getPostData _ = return Nothing

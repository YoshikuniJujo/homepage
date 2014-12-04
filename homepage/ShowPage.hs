{-# LANGUAGE FlexibleContexts, PackageImports, OverloadedStrings #-}

module ShowPage (showPage) where

import "monads-tf" Control.Monad.Trans (MonadIO, liftIO)
import Data.HandleLike (HandleLike, HandleMonad)
import Data.Pipe (Pipe)
import System.Directory (getModificationTime)
import System.FilePath (takeExtension, takeBaseName, (</>), addTrailingPathSeparator)
import Network.TigHTTP.Server (getRequest, requestPath, putResponse, response)
import Network.TigHTTP.Types (
	Path(..), Response(..),
	ContentType(..), Type(..), Subtype(..), ContentLength(..))

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as LBS

import MailToMe (mailFromBS)
import Tools (makePage, readBinaryFile, getPostData)

showPage :: (HandleLike h, MonadIO (HandleMonad h)) =>
	String -> h -> HandleMonad h ()
showPage ma p = do
	req <- getRequest p
	liftIO . print $ requestPath req
	getPostData req >>= liftIO . maybe (return ()) (mailFromBS ma)
	(page, tp, cl) <- liftIO . makeContents . takeWhile (/= '?')
		. BSC.unpack . (\(Path f) -> f) $ requestPath req
	putResponse p (responseH p $ LBS.fromChunks [page]) {
		responseContentType = tp,
		responseContentLength = cl }
	where
	responseH :: HandleLike h => h -> LBS.ByteString -> Response Pipe h
	responseH = const response

ico, png :: ContentType
ico = ContentType (TypeRaw "image") (SubtypeRaw "vnd.microsoft.icon") []
png = ContentType (TypeRaw "image") (SubtypeRaw "png") []

makeContents :: String -> IO (BSC.ByteString, ContentType, Maybe ContentLength)
makeContents fp_ = do
	let	fp = addIndex $ addPathSeparator fp_
		ex = takeExtension fp
		tp = case ex of
			".ico" -> ico
			".png" -> png
			".css" -> ContentType Text Css []
			_ -> ContentType Text Html []
	as <- liftIO $ if ex `elem` [".ico", ".png"]
		then readBinaryFile $ "static/" ++ fp
		else readFile $ "static/" ++ fp
	t <- liftIO . getModificationTime $ "static/" ++ fp
	let	
		page = if ex == ".html"
			then uncurry (makePage fp_ t) $ span (/= '\n') as
			else as
		cl = if ex == ".html"
			then Nothing
			else Just . ContentLength $ length page
		page' = case (ex, take 7 fp) of
			(".ico", _) -> BSC.pack page
			(".png", _) -> BSC.pack page
			(_, "/google") -> BSC.pack as
			_ -> BSU.fromString page
	return (page', tp, cl)

addIndex, addPathSeparator :: FilePath -> FilePath
addIndex fp
	| null $ takeBaseName fp = fp </> "index.html"
	| otherwise = fp

addPathSeparator fp
	| null $ takeExtension fp = addTrailingPathSeparator fp
	| otherwise = fp

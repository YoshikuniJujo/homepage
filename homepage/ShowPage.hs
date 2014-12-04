{-# LANGUAGE FlexibleContexts, PackageImports, OverloadedStrings #-}

module ShowPage (showPage) where

import "monads-tf" Control.Monad.Trans (MonadIO, liftIO)
import Data.HandleLike (HandleLike, HandleMonad)
import Data.Pipe (Pipe)
import System.Directory (getModificationTime)
import System.FilePath (takeExtension, takeBaseName, (</>), addTrailingPathSeparator)
import Network.TigHTTP.Server (getRequest, requestPath, putResponse, response)
import Network.TigHTTP.Types (
	Path(..), Response(..), ContentType(..), Type(..), Subtype(..))

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
	(page, tp) <- liftIO . makeContents . takeWhile (/= '?')
		. BSC.unpack . (\(Path f) -> f) $ requestPath req
	putResponse p
		(responseH p $ LBS.fromChunks [page]) { responseContentType = tp }
	where
	responseH :: HandleLike h => h -> LBS.ByteString -> Response Pipe h
	responseH = const response

ico, png, css, html, plain, octet :: ContentType
ico = ContentType (TypeRaw "image") (SubtypeRaw "vnd.microsoft.icon") []
png = ContentType (TypeRaw "image") (SubtypeRaw "png") []
css = ContentType Text Css []
html = ContentType Text Html []
plain = ContentType Text Plain []
octet = ContentType (TypeRaw "application") (SubtypeRaw "octet-stream") []

isBinary :: ContentType -> Bool
isBinary = (`elem` [ico, png, octet])

makeContents :: String -> IO (BSC.ByteString, ContentType)
makeContents fp_ = do
	let	fp = addIndex $ addPathSeparator fp_
		tp = case takeExtension fp of
			".ico" -> ico; ".png" -> png; ".css" -> css; ".html" -> html
			".hs" -> plain
			_ -> octet
	as <- liftIO $ if isBinary tp
		then readBinaryFile $ "static/" ++ fp
		else readFile $ "static/" ++ fp
	time <- liftIO . getModificationTime $ "static/" ++ fp
	let	
		page = if tp == html
			then uncurry (makePage fp_ time) $ span (/= '\n') as
			else as
		page' = case (isBinary tp, take 7 fp) of
			(True, _) -> BSC.pack page
			(_, "/google") -> BSC.pack as
			_ -> BSU.fromString page
	return (page', tp)

addIndex, addPathSeparator :: FilePath -> FilePath
addIndex fp
	| null $ takeBaseName fp = fp </> "index.html"
	| otherwise = fp

addPathSeparator fp
	| null $ takeExtension fp = addTrailingPathSeparator fp
	| otherwise = fp

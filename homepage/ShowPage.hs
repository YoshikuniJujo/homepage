{-# LANGUAGE FlexibleContexts, PackageImports, OverloadedStrings #-}

module ShowPage (showPage) where

import Control.Applicative ((<$>), (<*>))
import "monads-tf" Control.Monad.Trans (MonadIO, liftIO)
import Data.HandleLike (HandleLike, HandleMonad)
import Data.Pipe (Pipe)
import Data.Time (UTCTime, TimeZone(..), utcToZonedTime, formatTime)
import System.Locale (defaultTimeLocale)
import System.Directory (getModificationTime)
import System.FilePath (splitPath, dropTrailingPathSeparator)
import Network.TigHTTP.Server (getRequest, requestPath, putResponse, response)
import Network.TigHTTP.Types (Path(..), Response(..), ContentType)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as LBS

import MailToMe (mailFromBS)
import Tools (
	readBinaryFile, getPostData, html, contentType, isBinary,
	addIndex, addPathSeparator)

showPage :: (HandleLike h, MonadIO (HandleMonad h)) =>
	String -> h -> HandleMonad h ()
showPage ma hdl = do
	req <- getRequest hdl
	liftIO . print $ requestPath req
	getPostData req >>= liftIO . maybe (return ()) (mailFromBS ma)
	let	fp_ = takeWhile (/= '?')
			. BSC.unpack . (\(Path f) -> f) $ requestPath req
		fp = "static/" ++ addIndex (addPathSeparator fp_)
		tp = contentType fp
	ft <- liftIO $ readFile "css_checked.txt"
	(cnt, mt) <- liftIO $ (,)
		<$> (if isBinary tp then readBinaryFile else readFile) fp
		<*> getModificationTime fp
	putResponse hdl
		(responseH hdl $ LBS.fromChunks [createContents fp_ mt tp ft cnt])
			{ responseContentType = tp }
	where
	responseH :: HandleLike h => h -> LBS.ByteString -> Response Pipe h
	responseH = const response

createContents ::
	FilePath -> UTCTime -> ContentType -> String -> String -> BSC.ByteString
createContents fp_ mt tp ft cnt =
	let	page = if tp == html
			then uncurry (makeHtml fp_ mt ft) $ span (/= '\n') cnt
			else cnt
		page' = case (isBinary tp, take 7 fp_) of
			(True, _) -> BSC.pack page
			(_, "/google") -> BSC.pack cnt
			_ -> BSU.fromString page
	in page'

makeHtml :: FilePath -> UTCTime -> String -> String -> String -> String
makeHtml fp mt ft ttl bdy = "<!DOCTYPE html><html lang=\"ja\"><head>"
	++ "<meta charset=\"UTF-8\"><title>" ++ ttl ++ "</title>"
	++ "<link href=\"/css/basic.css\" rel=\"stylesheet\" type=\"text/css\">"
	++ "</head><body>"
	++ "<small>"
	++ (if fp /= "/" then "<a href=\"/\">top</a> > " else "")
	++ concat (tail' $ map (uncurry pathToHref) (init $ getPaths fp))
	++ (last' . map fst $ getPaths fp)
	++ "</small>"
	++ "<div class=\"right\"><small>更新日: <time>"
		++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M+09:00" (utcToZonedTime japan mt)
		++ "</time></small></div>"
	++ "<div class=\"right\"><small>文責: 重城良国</small></div>"
	++ "<h1>" ++ ttl ++ "</h1>"
	++ filter (/= '\n') bdy
	++ "<p class=\"right\"><a href=\"" ++ pathToCheckerUri fp
	++ "\"><img src=\"http://jigsaw.w3.org/css-validator/images/vcss\" width=\"44\" height=\"15.5\" alt=\"正当なCSSです!\"/></a>"
	++ " <a href=\"" ++ pathToCheckerUri fp
	++ "\"><img src=\"http://www.w3.org/html/logo/badge/html5-badge-h-css3-semantics.png\" width=\"41.25\" height=\"16\" alt=\"HTML5 Powered with CSS3 / styling, and Semantics\"/></a></p>"
	++ "</body></html>"
	where
	japan :: TimeZone
	japan = TimeZone 540 False "JST"

	pathToHref :: String -> FilePath -> String
	pathToHref n p = "<a href=\"" ++ p ++ "\">" ++ n ++ "</a> > "

	getPaths :: FilePath -> [(String, FilePath)]
	getPaths p = zip (map dropTrailingPathSeparator ps) (tail' $ scanl (++) "" ps)
		where
		ps = splitPath p

	tail' :: [a] -> [a]
	tail' [] = []
	tail' (_ : t) = t

	last' :: [String] -> String
	last' [] = ""
	last' [_] = "top"
	last' xs = last xs

pathToCssCheckerUri :: FilePath -> String
pathToCssCheckerUri =
	("http://jigsaw.w3.org/css-validator/validator?uri=http%3A%2F%2Fskami.iocikun.jp" ++)
		. escapeSlash

pathToCheckerUri :: FilePath -> String
pathToCheckerUri =
	("http://validator.w3.org/check?uri=http%3A%2F%2Fskami.iocikun.jp" ++)
		. escapeSlash

escapeSlash :: FilePath -> String
escapeSlash "" = ""
escapeSlash ('/' : cs) = "%2F" ++ escapeSlash cs
escapeSlash (c : cs) = c : escapeSlash cs

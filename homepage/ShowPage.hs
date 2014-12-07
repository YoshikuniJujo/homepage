{-# LANGUAGE FlexibleContexts, PackageImports #-}

module ShowPage (showPage) where

import Control.Applicative ((<$>), (<*>))
import "monads-tf" Control.Monad.Trans (MonadIO, liftIO)
import Data.List (isPrefixOf)
import Data.Char (isAscii)
import Data.Time (UTCTime, TimeZone(..), utcToZonedTime, formatTime)
import Data.HandleLike (HandleLike, HandleMonad)
import Data.Pipe (Pipe)
import System.Directory (getModificationTime)
import System.FilePath (splitPath, dropTrailingPathSeparator)
import System.Locale (defaultTimeLocale)
import Network.TigHTTP.Server (getRequest, requestPath, putResponse, response)
import Network.TigHTTP.Types (Path(..), Response(..), ContentType)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as LBS

import MailToMe (mailTo)
import Tools (
	readBinaryFile, addIndex, addPathSeparator,
	getPostData, contentType, isHtml, isBinary )

showPage :: (HandleLike h, MonadIO (HandleMonad h)) =>
	String -> h -> HandleMonad h ()
showPage ma hdl = do
	req <- getRequest hdl
	liftIO . print $ requestPath req
	getPostData req >>= liftIO . maybe (return ()) (mailTo ma)
	let	fp_ = takeWhile (/= '?')
			. BSC.unpack . (\(Path f) -> f) $ requestPath req
		fp = "static/" ++ addIndex (addPathSeparator fp_)
		tp = contentType fp
	(cnt, mt) <- liftIO $ (,)
		<$> (if isBinary tp then readBinaryFile else readFile) fp
		<*> getModificationTime fp
	putResponse hdl (responseH hdl $ LBS.fromChunks [makePage fp_ mt tp cnt])
		{ responseContentType = tp }
	where
	responseH :: HandleLike h => h -> LBS.ByteString -> Response Pipe h
	responseH = const response

makePage :: FilePath -> UTCTime -> ContentType -> String -> BSC.ByteString
makePage fp_ mt tp cnt = case (isHtml tp, isBinary tp, isGoogleCheck fp_) of
	(True, _, False) -> BSU.fromString . makeHtml fp_ mt $ span (/= '\n') cnt
	(_, False, False) -> BSU.fromString cnt
	_ -> BSC.pack cnt
	where isGoogleCheck = ("/google89" `isPrefixOf`)

makeHtml :: FilePath -> UTCTime -> (String, String) -> String
makeHtml fp mt (ttl, bdy) = "<!DOCTYPE html><html lang=\"ja\">" ++ makeHead ttl
	++ "<body>" ++ breadcrumb (splitPath fp) ++ makeMeta mt "重城良国"
	++ "<h1>" ++ ttl ++ "</h1>"
	++ removeNewline False bdy
	++ makeValidIcons fp
	++ "</body></html>"

makeHead :: String -> String
makeHead ttl = "<head><meta charset=\"UTF-8\"><title>" ++ ttl ++ "</title>"
	++ "<link href=\"/css/basic.css\" rel=\"stylesheet\" type=\"text/css\">"
	++ "</head>"

breadcrumb :: [String] -> String
breadcrumb ["/"] = "<small>top</small>"
breadcrumb ("/" : ps) = "<small><a href=\"/\">top</a> &gt; "
	++ concatMap link (init pairs) ++ last (map fst pairs) ++ "</small>"
	where
	link (n, p) = "<a href=\"" ++ p ++ "\">" ++ n ++ "</a> &gt; "
	pairs = ($ ps) $ zip
		<$> map dropTrailingPathSeparator <*> (tail . scanl (++) "/")
breadcrumb _ = "<small>bad path</small>"

makeMeta :: UTCTime -> String -> String
makeMeta mt at = "<div class=\"right\"><small>更新日: <time>"
	++ formatTime defaultTimeLocale
		"%Y-%m-%dT%H:%M+09:00" (utcToZonedTime japan mt)
	++ "</time></small></div>"
	++ "<div class=\"right\"><small>文責: " ++ at ++ "</small></div>"
	where japan = TimeZone 540 False "JST"

removeNewline :: Bool -> String -> String
removeNewline _ "" = ""
removeNewline j ('\n' : c : cs)
	| j || not (isAscii c) = c : removeNewline (not $ isAscii c) cs
removeNewline _ (c : cs) = c : removeNewline (not $ isAscii c) cs

makeValidIcons :: FilePath -> String
makeValidIcons fp = "<p class=\"right\"><a href=\"" ++ pathToCssCk fp ++ "\">"
	++ "<img src=\"/images/vcss.png\" "
	++ "width=\"44\" height=\"15\" alt=\"正当なCSSです!\"/></a>"
	++ " <a href=\"" ++ pathToCk fp ++ "\">"
	++ "<img src=\"/images/"
	++ "html5-badge-h-css3-semantics.png\" "
	++ "width=\"41\" height=\"16\" "
	++ "alt=\"HTML5 Powered with CSS3 / styling, and Semantics\"/></a>"
	++ "</p>"

pathToCssCk :: FilePath -> String
pathToCssCk = ("http://jigsaw.w3.org/css-validator/validator" ++) . pathToGetUri

pathToCk :: FilePath -> String
pathToCk = ("http://validator.w3.org/check" ++) . pathToGetUri

pathToGetUri :: FilePath -> String
pathToGetUri = ("?uri=http%3A%2F%2Fskami.iocikun.jp" ++) . escapeSlash
	where
	escapeSlash "" = ""
	escapeSlash ('/' : cs) = "%2F" ++ escapeSlash cs
	escapeSlash (c : cs) = c : escapeSlash cs

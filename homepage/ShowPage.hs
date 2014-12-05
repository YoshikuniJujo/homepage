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

import MailToMe (mailFromBS)
import Tools (
	readBinaryFile, addIndex, addPathSeparator,
	getPostData, contentType, isHtml, isBinary )

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
	(cnt, mt) <- liftIO $ (,)
		<$> (if isBinary tp then readBinaryFile else readFile) fp
		<*> getModificationTime fp
	putResponse hdl
		(responseH hdl $ LBS.fromChunks [createContents fp_ mt tp cnt])
			{ responseContentType = tp }
	where
	responseH :: HandleLike h => h -> LBS.ByteString -> Response Pipe h
	responseH = const response

createContents ::
	FilePath -> UTCTime -> ContentType -> String -> BSC.ByteString
createContents fp_ mt tp cnt = case (isHtml tp, isBinary tp, isGoogleCheck fp_) of
	(True, _, False) -> BSU.fromString . makeHtml fp_ mt $ span (/= '\n') cnt
	(_, False, False) -> BSU.fromString cnt
	_ -> BSC.pack cnt
	where isGoogleCheck = ("/google89" `isPrefixOf`)

makeHtml :: FilePath -> UTCTime -> (String, String) -> String
makeHtml fp mt (ttl, bdy) = "<!DOCTYPE html><html lang=\"ja\">" ++ makeHead ttl
	++ "<body>" ++ breadcrumb (splitPath fp) ++ makeMeta mt
	++ "<h1>" ++ ttl ++ "</h1>"
	++ removeNewline False bdy ++ makeValidIcons fp ++ "</body></html>"

makeHead :: String -> String
makeHead ttl = "<head><meta charset=\"UTF-8\"><title>" ++ ttl ++ "</title>"
	++ "<link href=\"/css/basic.css\" rel=\"stylesheet\" type=\"text/css\">"
	++ "</head>"

breadcrumb :: [String] -> String
breadcrumb ["/"] = "<small>top</small>"
breadcrumb ("/" : ps) = "<small><a href=\"/\">top</a> &gt; "
	++ concat (map pathToHref $ init pairs) ++ last (map fst pairs)
	++ "</small>"
	where
	pathToHref (n, p) = "<a href=\"" ++ p ++ "\">" ++ n ++ "</a> &gt; "
	pairs = zip
		<$> map dropTrailingPathSeparator
		<*> (tail . scanl (++) "/") $ ps
breadcrumb _ = "<small>bad path</small>"

makeMeta :: UTCTime -> String
makeMeta mt = "<div class=\"right\"><small>更新日: <time>"
	++ formatTime defaultTimeLocale "%Y-%m-%dT%H:%M+09:00" (utcToZonedTime japan mt)
	++ "</time></small></div>"
	++ "<div class=\"right\"><small>文責: 重城良国</small></div>"
	where
	japan :: TimeZone
	japan = TimeZone 540 False "JST"

removeNewline :: Bool -> String -> String
removeNewline _ "" = ""
removeNewline j ('\n' : c : cs)
	| j || not (isAscii c) = c : removeNewline (not $ isAscii c) cs
removeNewline _ (c : cs) = c : removeNewline (not $ isAscii c) cs

makeValidIcons :: FilePath -> String
makeValidIcons fp = "<p class=\"right\"><a href=\"" ++ pathToCssCheckerUri fp
	++ "\"><img src=\"http://jigsaw.w3.org/css-validator/images/vcss\" width=\"44\" height=\"15\" alt=\"正当なCSSです!\"/></a>"
	++ " <a href=\"" ++ pathToCheckerUri fp
	++ "\"><img src=\"http://www.w3.org/html/logo/badge/html5-badge-h-css3-semantics.png\" width=\"41\" height=\"16\" alt=\"HTML5 Powered with CSS3 / styling, and Semantics\"/></a></p>"

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

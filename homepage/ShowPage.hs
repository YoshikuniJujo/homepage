{-# LANGUAGE OverloadedStrings, FlexibleContexts, PackageImports #-}

module ShowPage (showPage, initLock) where

import Control.Applicative ((<$>), (<*>))
import "monads-tf" Control.Monad.Trans (MonadIO, liftIO)
import Control.Concurrent.STM (TVar)
import Data.Maybe (fromMaybe, fromJust)
import Data.List (isPrefixOf)
import Data.Char (isAscii)
import Data.Time (
	UTCTime, TimeZone(..), utcToZonedTime, formatTime, getZonedTime,
	getCurrentTime)
import Data.HandleLike (HandleLike, HandleMonad)
import Data.Pipe (Pipe)
import System.IO (IOMode(..), openBinaryFile, hGetContents)
import System.Directory (getModificationTime, doesFileExist)
import System.FilePath (splitPath, dropTrailingPathSeparator)
import System.Locale (defaultTimeLocale)
import Network.TigHTTP.Server (getRequest, requestPath, putResponse, response)
import Network.TigHTTP.Types -- (Path(..), Response(..), ContentType)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as LBS

import MailToMe (mailTo)
import Tools (
	addIndex, addSep, getPostData, contentType, isHtml, isBinary, html )
import StmFile (update)
import StmLock (Lock, initLock)

userAgent :: Request h -> Maybe [Product]
userAgent (RequestGet _ _ gt) = getUserAgent gt
userAgent (RequestPost _ _ pst) = postUserAgent pst
userAgent _ = Nothing

showProduct :: Product -> String
showProduct (Product p v) =
	BSC.unpack p ++ "" `fromMaybe` ((' ' :) . BSC.unpack <$> v)
showProduct (ProductComment c) = BSC.unpack c

testProducts :: [([Product], String)]
testProducts = [
	([
		Product "Mozilla" (Just "5.0"),
		ProductComment "X11; Linux i686; rv:24.0",
		Product "Gecko" (Just "20141006"),
		Product "Firefox" (Just "24.0") ], "PC"),
	([
		Product "Mozilla" (Just "5.0"),
		ProductComment "Android; Mobile; rv:35.0",
		Product "Gecko" (Just "35.0"),
		Product "Firefox" (Just "35.0") ], "Phone")
	]

{-
showRequest :: Request h -> String
showRequest (RequestGet p v gt) =
	"RequestGet " ++ show p ++ " " ++ show v ++ " (" ++ show gt ++ ")"
showRequest (RequestPost p v pst) =
	"RequestPost " ++ show p ++ " " ++ show v ++ " (" ++ showPost pst ++ ")"
showRequest (RequestRaw rt p v rw) =
	"RequestRaw " ++ show rt ++ " " ++ show p ++ show v ++ " " ++ show rw

showPost :: Post h -> String
showPost _ = "comming soon"
-}

showPath :: Path -> String
showPath (Path p) = BSC.unpack p

showPage :: (HandleLike h, MonadIO (HandleMonad h)) =>
	TVar Lock -> String -> h -> HandleMonad h ()
showPage l ma hdl = do
	req <- getRequest hdl
--	liftIO . putStrLn $ showRequest req
	liftIO $ do
		now <- getZonedTime
		let lg = show now ++ '\t' : showPath (requestPath req) ++
			case userAgent req of
				Just agent -> if agent `elem` map fst testProducts
					then " " ++ fromJust (lookup agent testProducts)
					else ("\n\t" ++) . unwords $ map showProduct agent
				_ -> ""
		update l "tmp" "log/access.log" $
			(`BSC.append` "\n") . (`BSC.append` BSC.pack lg)
	getPostData req >>= liftIO . maybe (return ()) (mailTo ma)
	let	fp_ = takeWhile (/= '?')
			. BSC.unpack . (\(Path f) -> f) $ requestPath req
		fp = "static/" ++ addIndex (addSep fp_)
		tp = contentType fp
	fe <- liftIO $ doesFileExist fp
	if fe	then do	(cnt, mt) <- liftIO $ (,)
				<$> (if isBinary tp
					then readBinaryFile else readFile) fp
				<*> getModificationTime fp
			putResponse hdl (responseH hdl
					$ LBS.fromChunks [makePage fp_ mt tp cnt])
				{ responseContentType = tp }
		else do	now <- liftIO getCurrentTime
			putResponse hdl (responseH hdl
					$ LBS.fromChunks [makePage "/404" now html
						"404 File not found" ]) {
				responseStatusCode = NotFound,
				responseContentType = html
				}
	where
	readBinaryFile path = openBinaryFile path ReadMode >>= hGetContents
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
	++ "<meta name=\"viewport\" content=\"width=device-width\""
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

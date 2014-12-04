{-# LANGUAGE FlexibleContexts, PackageImports, OverloadedStrings #-}

module ShowPage (showPage) where

import Control.Monad (liftM)
import "monads-tf" Control.Monad.Trans (MonadIO, liftIO)
import Data.HandleLike (HandleLike, HandleMonad)
import Data.Pipe (Pipe, runPipe, (=$=))
import Data.Pipe.List (toList)
import Data.URLEncoded (URLEncoded, importString, (%!))
import System.IO (IOMode(..), openBinaryFile, hGetContents)
import System.Directory (getModificationTime)
import System.FilePath (takeExtension)
import Network.TigHTTP.Server (getRequest, requestPath, putResponse, response)
import Network.TigHTTP.Types (
	Request(..), Path(..), Post(..), Response(..),
	ContentType(..), Type(..), Subtype(..), ContentLength(..))
import Network.Mail.SMTP (Address(..), sendMail, simpleMail, plainTextPart)

import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as LBS
import qualified Data.Text as T
import qualified Data.Text.Lazy as LT

import Tools (makePage, processIndex)

showPage :: (HandleLike h, MonadIO (HandleMonad h)) =>
	String -> h -> HandleMonad h ()
showPage addr p = do
	req <- getRequest p
	mailFromForm req addr
	(page', tp, cl) <- liftIO $ makeContents req
	let	rsp = (responseH p page') {
			responseContentType = tp,
			responseContentLength = cl }
	putResponse p rsp

makeContents :: Request h -> IO (LBS.ByteString, ContentType, Maybe ContentLength)
makeContents req = do
	let	Path fp__ = requestPath req
		fp_ = takeWhile (/= '?') $ BSC.unpack fp__
		fp = processIndex fp_
		ex = takeExtension fp
		stp = if ex == ".css" then Css else Html
		tp = case ex of
			".ico" -> ContentType
				(TypeRaw "image")
				(SubtypeRaw "vnd.microsoft.icon") []
			".png" -> ContentType
				(TypeRaw "image")
				(SubtypeRaw "png") []
			_ -> ContentType Text stp []
	liftIO $ print fp__


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
			(".ico", _) -> LBS.fromChunks [BSC.pack page]
			(".png", _) -> LBS.fromChunks [BSC.pack page]
			(_, "/google") -> LBS.fromChunks [BSC.pack as]
			_ -> LBS.fromChunks [BSU.fromString page]
	return (page', tp, cl)

responseH :: HandleLike h => h -> LBS.ByteString -> Response Pipe h
responseH = const response

readBinaryFile :: FilePath -> IO String
readBinaryFile path = openBinaryFile path ReadMode >>= hGetContents

getPostData :: HandleLike h => Request h -> HandleMonad h (Maybe [BSC.ByteString])
getPostData (RequestPost _ _ Post { postBody = pb }) = runPipe $ pb =$= toList
getPostData (RequestGet (Path p) _ _)
	| BSC.null rtn = return Nothing
	| otherwise = return . Just . (: []) $ myTail rtn
	where
	rtn = BSC.dropWhile (/= '?') p
	myTail "" = ""
	myTail bs = BSC.tail bs
getPostData _ = return Nothing

mailFromForm :: (HandleLike h, MonadIO (HandleMonad h)) =>
	Request h -> String -> HandleMonad h ()
mailFromForm req addr = do
	ret <- fmap BSC.concat `liftM` getPostData req
	liftIO $ case ret of
		Just r -> mailFromBS addr r
		_ -> return ()

mailFromBS :: String -> BSU.ByteString -> IO ()
mailFromBS addr r = do
	ue <- importString $ BSC.unpack r
	maybe (return ()) (\nazo -> do
			putStrLn nazo
			mailTo "Administer" (T.pack addr) "nazo" $ LT.pack nazo
		) $ makeMailBody ue

makeMailBody :: URLEncoded -> Maybe String
makeMailBody ue =
	case (ue %! ("name" :: String), ue %! ("address" :: String),
			ue %! ("body" :: String)) of
		(Just nm, Just addr, Just bdy) -> Just $ "お名前: " ++ nm ++
			"\nメールアドレス: " ++ addr ++ "\n\n内容:\n" ++ bdy
		_ -> Nothing

mailTo :: T.Text -> T.Text -> T.Text -> LT.Text -> IO ()
mailTo n a t b = sendMail "skami.iocikun.jp" $ simpleMail
	(Address (Just "Homepage") "tatsuya@skami.iocikun.jp")
	[Address (Just n) a] [] [] t [plainTextPart b]

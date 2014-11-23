{-# LANGUAGE FlexibleContexts, PackageImports, OverloadedStrings #-}

module ShowPage (showPage) where

import Control.Monad
import "monads-tf" Control.Monad.Trans
import Data.HandleLike
import Data.Pipe
import Data.Pipe.List
import Data.URLEncoded
import System.Directory
import System.FilePath
import Network.TigHTTP.Server
import Network.TigHTTP.Types
import Network.Socket
import Network.Mail.Mime (Mail)
import Network.Mail.SMTP hiding (Response)

import qualified Data.Text as T
import qualified Data.Text.Lazy as LT
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as LBS

import Tools

import System.Environment

showPage :: (HandleLike h, MonadIO (HandleMonad h)) => h -> HandleMonad h ()
showPage p = do
	_ : addr : _ <- liftIO getArgs
	req <- getRequest p
	let	Path fp__ = requestPath req
		fp_ = takeWhile (/= '?') $ BSC.unpack fp__
		fp = processIndex fp_
		ex = takeExtension $ fp
		stp = if ex == ".css" then Css else Html
		tp = ContentType Text stp []
	liftIO $ print fp__

	mailFromForm req addr

--	getPostData req >>= liftIO . maybe (return ()) (print . BSC.concat)
	as <- liftIO . readFile $ "static/" ++ fp
	t <- liftIO . getModificationTime $ "static/" ++ fp
	let	
		page = if ex == ".html"
			then uncurry (makePage fp_ t) $ span (/= '\n') as
			else as
	putResponse p $ (responseH p $
		LBS.fromChunks [BSU.fromString page]) { responseContentType = tp }

mailFromForm :: (HandleLike h, MonadIO (HandleMonad h)) =>
	Request h -> String -> HandleMonad h ()
mailFromForm req addr = do
	ret <- (fmap BSC.concat) `liftM` getPostData req
	liftIO $ case ret of
		Just r -> do
			ue <- importString $ BSC.unpack r
			maybe (return ()) (\nazo -> do
					putStrLn nazo
					mailTo "Administer" (T.pack addr)
						"nazo" $ LT.pack nazo
				) $ makeMailBody ue -- ue %! ("address" :: String)
		_ -> return ()

makeMailBody :: URLEncoded -> Maybe String
makeMailBody ue =
	case (ue %! ("name" :: String), ue %! ("address" :: String),
			ue %! ("body" :: String)) of
		(Just nm, Just addr, Just bdy) -> Just $ "お名前: " ++ nm ++
			"\nメールアドレス: " ++ addr ++ "\n\n内容:\n" ++ bdy
		_ -> Nothing

responseH :: HandleLike h => h -> LBS.ByteString -> Response Pipe h
responseH = const response

getPostData :: HandleLike h => Request h -> HandleMonad h (Maybe [BSC.ByteString])
getPostData (RequestPost _ _ Post { postBody = pb }) = runPipe $ pb =$= toList
getPostData (RequestGet (Path p) _ _)
	| BSC.null rtn = return Nothing
	| otherwise = return . Just . (: []) $ myTail rtn
	where
	rtn = BSC.dropWhile (/= '?') p
getPostData _ = return Nothing

myTail :: BSC.ByteString -> BSC.ByteString
myTail "" = ""
myTail bs = BSC.tail bs

mailTo :: T.Text -> T.Text -> T.Text -> LT.Text -> IO ()
mailTo n a t b = sendMail "skami.iocikun.jp" $ simpleMail
	(Address (Just "Homepage") "tatsuya@skami.iocikun.jp")
	[Address (Just n) a] [] [] t [plainTextPart b]

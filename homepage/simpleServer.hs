{-# LANGUAGE OverloadedStrings, PackageImports, FlexibleContexts #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import Control.Concurrent
import Data.HandleLike
import Data.Pipe
import System.IO
import System.Environment
import System.FilePath
import System.Posix.User
import Network
import Network.PeyoTLS.Server
import Network.PeyoTLS.ReadFile
import Network.TigHTTP.Server
import Network.TigHTTP.Types
import "crypto-random" Crypto.Random

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.UTF8 as BSU
import qualified Data.ByteString.Lazy as LBS

import Tools

main :: IO ()
main = do
	d : _ <- getArgs
	k <- readKey $ d ++ "/private_2014.key"
	c <- readCertificateChain [d ++ "/2014_only_skami.cert", d ++ "/owl_mid.pem"]
	g0 <- cprgCreate <$> createEntropyPool :: IO SystemRNG
	soc <- listenOn $ PortNumber 443
	uid <- userID <$> getUserEntryForName "homepage"
	gid <- groupID <$> getGroupEntryForName "homepage"
	setGroupID gid
	setUserID uid
	void . (`runStateT` g0) . forever $ do
		(h, _, _) <- liftIO $ accept soc
		g <- StateT $ return . cprgFork
		liftIO . forkIO . (`run` g) $ do
			p <- open h [
					"TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256",
					"TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA",
					"TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256",
					"TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA",
					"TLS_DHE_RSA_WITH_AES_128_CBC_SHA256",
					"TLS_DHE_RSA_WITH_AES_128_CBC_SHA",
					"TLS_RSA_WITH_AES_128_CBC_SHA256",
					"TLS_RSA_WITH_AES_128_CBC_SHA"
				] [(k, c)]
				Nothing

			showPage p
			hlClose p

doUntil :: Monad m => (a -> Bool) -> m a -> m [a]
doUntil p rd = rd >>= \x ->
	(if p x then return . (: []) else (`liftM` doUntil p rd) . (:)) x

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

showPage_ :: (HandleLike h, MonadIO (HandleMonad h)) => h -> HandleMonad h ()
showPage_ p = do
			doUntil BS.null (hlGetLine p) >>= liftIO . mapM_ BSC.putStrLn
			hlPut p $ BS.concat [
				"HTTP/1.1 200 OK\r\n",
				"Transfer-Encoding: chunked\r\n",
				"Content-Type: text/plain\r\n\r\n",
				"5\r\nHello0\r\n\r\n" ]


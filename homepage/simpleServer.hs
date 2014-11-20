{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Applicative
import Control.Monad
import "monads-tf" Control.Monad.State
import Control.Concurrent
import Data.HandleLike
import System.Environment
import System.Posix.User
import Network
import Network.PeyoTLS.Server
import Network.PeyoTLS.ReadFile
import "crypto-random" Crypto.Random

import ShowPage

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

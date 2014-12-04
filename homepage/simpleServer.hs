{-# LANGUAGE OverloadedStrings, PackageImports, ScopedTypeVariables #-}

module Main (main) where

import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad (void, forever)
import "monads-tf" Control.Monad.State (StateT(..), runStateT, liftIO)
import Control.Concurrent (forkIO)
import Data.HandleLike (hlClose)
import System.Environment (getArgs)
import Network (PortID(..), listenOn, accept)
import Network.PeyoTLS.Server (CipherSuite, run, open)
import Network.PeyoTLS.ReadFile (readKey, readCertificateChain)
import "crypto-random" Crypto.Random (
	SystemRNG, createEntropyPool, cprgCreate, cprgFork)

import ShowPage (showPage)
import Tools (setHomepageID)

keyFile :: String
keyFile = "../certs/private_2014.key"

certFile :: [String]
certFile = ["../certs/2014_only_skami.cert", "../certs/owl_mid.pem"]

cipherSuites :: [CipherSuite]
cipherSuites = [
	"TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA256",
	"TLS_ECDHE_ECDSA_WITH_AES_128_CBC_SHA",
	"TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA256",
	"TLS_ECDHE_RSA_WITH_AES_128_CBC_SHA",
	"TLS_DHE_RSA_WITH_AES_128_CBC_SHA256",
	"TLS_DHE_RSA_WITH_AES_128_CBC_SHA",
	"TLS_RSA_WITH_AES_128_CBC_SHA256",
	"TLS_RSA_WITH_AES_128_CBC_SHA" ]

main :: IO ()
main = do
	addr : _ <- getArgs
	kc <- (,) <$> readKey keyFile <*> readCertificateChain certFile
	soc <- listenOn (PortNumber 443) <* setHomepageID
	g0 :: SystemRNG <- cprgCreate <$> createEntropyPool
	void . (`runStateT` g0) . forever $ do
		g <- StateT $ return . cprgFork
		liftIO $ do
			(h, _, _) <- accept soc
			forkIO . (`run` g) $ open h cipherSuites [kc] Nothing >>=
				(>>) <$> showPage addr <*> hlClose

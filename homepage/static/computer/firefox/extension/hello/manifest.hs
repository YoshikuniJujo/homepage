{-# LANGUAGE OverloadedStrings #-}

import System.Directory

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Data.ByteString.Base64 as Base64
import qualified Crypto.Hash.MD5 as MD5
import qualified Crypto.Hash.SHA1 as SHA1

main :: IO ()
main = do
	setCurrentDirectory ".."
	getDirectoryContents "." >>= print
	mnf <- mkSection "defaults/preferences/Lightening.js"
	BS.putStr mnf
	BSC.putStrLn . Base64.encode $ MD5.hash mnf
	BSC.putStrLn . Base64.encode $ SHA1.hash mnf

mkSection :: FilePath -> IO BS.ByteString
mkSection fp = do
	cnt <- BS.readFile fp
	return $ "Name: " `BS.append` BSC.pack fp `BS.append` "\n"
		`BS.append` "Digest-Algorithms: MD5 SHA1\nMD5-Digest: "
		`BS.append` Base64.encode (MD5.hash cnt)
		`BS.append` "\nSHA1-Digest: "
		`BS.append` Base64.encode (SHA1.hash cnt)
		`BS.append` "\n"

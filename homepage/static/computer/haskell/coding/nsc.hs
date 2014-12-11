{-# LANGUAGE OverloadedStrings #-}

import Control.Applicative
import Control.Arrow
import System.Random
import Crypto.Cipher.AES
import Crypto.Cipher.Types

import qualified Data.ByteString as BS
import qualified Crypto.Hash.SHA256 as SHA256

key :: AES
key = initAES ("passwordpassword" :: BS.ByteString)

iv :: BS.ByteString
iv = "this is iv ivivi"

randomIv :: RandomGen g => AES -> g -> (BS.ByteString, g)
randomIv a g = BS.pack `first` times (blockSize a) random g

times :: Int -> (s -> (x, s)) -> s -> ([x], s)
times n _ s | n <= 0 = ([], s)
times n f s = let
	(x, s') = f s
	(xs, s'') = times (n - 1) f s' in
	(x : xs, s'')

mkAes :: BS.ByteString -> AES
mkAes = initAES . SHA256.hash

padToLen :: BS.ByteString -> Int -> BS.ByteString
padToLen s n = s `BS.append`
	BS.replicate (n - BS.length s) (fromIntegral $ n - BS.length s - 1)

blkSzToLen :: Int -> BS.ByteString -> Int
blkSzToLen b s = (BS.length s `div` b + 1) * b

padding :: Int -> BS.ByteString -> BS.ByteString
padding b = ($) <$> padToLen <*> blkSzToLen b

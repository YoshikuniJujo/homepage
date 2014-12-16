{-# LANGUAGE OverloadedStrings #-}

import Control.Arrow
import System.Random
import Crypto.Cipher.AES
import Crypto.Cipher.Types

import qualified Data.ByteString as BS

keySample :: AES
keySample = initAES ("passwordpassword" :: BS.ByteString)

ivSample :: BS.ByteString
ivSample = "this is iv ivivi"

times :: Int -> (s -> (x, s)) -> s -> ([x], s)
times n _ s | n <= 0 = ([], s)
times n f s = let
	(x, s') = f s
	(xs, s'') = times (n - 1) f s' in
	(x : xs, s'')

randomIv :: RandomGen g => AES -> g -> (BS.ByteString, g)
randomIv a g = BS.pack `first` times (blockSize a) random g

{-# LANGUAGE OverloadedStrings #-}

import Crypto.Cipher.AES
import Crypto.Cipher.Types

import qualified Data.ByteString as BS

keySample :: AES
keySample = initAES ("passwordpassword" :: BS.ByteString)

ivSample :: BS.ByteString
ivSample = "this is iv ivivi"

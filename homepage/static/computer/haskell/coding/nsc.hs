{-# LANGUAGE OverloadedStrings #-}

import Crypto.Cipher.AES
import Crypto.Cipher.Types

import qualified Data.ByteString as BS

key :: AES
key = initAES ("passwordpassword" :: BS.ByteString)

iv :: BS.ByteString
iv = "this is iv ivivi"

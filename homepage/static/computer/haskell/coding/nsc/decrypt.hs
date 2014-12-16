{-# LANGUAGE OverloadedStrings #-}

import Data.Maybe
import System.Environment

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Nsc

main :: IO ()
main = getArgs >>= BS.interact . (fromMaybe "" .)
	. decrypt . makeKey . BSC.pack . head

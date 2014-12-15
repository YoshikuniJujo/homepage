{-# LANGUAGE OverloadedStrings, PackageImports #-}

import Control.Arrow
import "monads-tf" Control.Monad.State
import "monads-tf" Control.Monad.Identity
import System.Random

import qualified Data.ByteString as BS

randomByteString ::
	RandomGen g => Int -> g -> (BS.ByteString, g)
randomByteString n g = BS.pack `first` times n random g

times :: Int -> (s -> (x, s)) -> s -> ([x], s)
times n f = runState . replicateM n . StateT $ Identity . f

{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad (void, forever)
import Control.Concurrent (forkIO)
import System.IO (hClose)
import System.Environment (getArgs)
import Network (PortID(..), listenOn, accept)

import ShowPage (showPage)
import Tools (setHomepageID)

main :: IO ()
main = do
	addr : _ <- getArgs
	soc <- listenOn (PortNumber 80) <* setHomepageID
	forever $ accept soc >>= void . forkIO
		. ((>>) <$> showPage addr <*> hClose) . (\(h, _, _) -> h)

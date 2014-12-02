{-# LANGUAGE OverloadedStrings #-}

module Main (main) where

import Control.Applicative ((<$>), (<*>), (<*))
import Control.Monad (void, forever)
import Control.Concurrent (forkIO)
import System.IO (hClose)
import Network (PortID(..), listenOn, accept)

import ShowPage (showPage)
import Tools (setHomepageID)

main :: IO ()
main = do
	soc <- listenOn (PortNumber 80) <* setHomepageID
	forever $ accept soc >>=
		void . forkIO . ((>>) <$> showPage <*> hClose) . (\(h, _, _) -> h)

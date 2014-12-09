module StmFile (update) where

import Control.Applicative
import Control.Concurrent
import Control.Concurrent.STM
import StmLock
import System.Directory
import Numeric

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC
import qualified Crypto.Hash.SHA256 as SHA256

update :: TVar Lock ->
	FilePath -> FilePath -> (BS.ByteString -> BS.ByteString) -> IO ()
update l td fp f = do
	dp <- (td ++) . ('/' :) . getDirPath <$> myThreadId
	createDirectory dp
--	putStrLn dp
	updateFile l dp fp f
	removeDirectory dp
	where
	getDirPath tid = convert '/' '_' (convert '.' '_' fp)
		++ "_" ++ tail (dropWhile (/= ' ') $ show tid)

convert :: Eq a => a -> a -> [a] -> [a]
convert pr pst = map $ \x -> if x == pr then pst else x

updateFile :: TVar Lock ->
	FilePath -> FilePath -> (BS.ByteString -> BS.ByteString) -> IO ()
updateFile l dp fp f = do
	cnt <- BS.readFile fp
	let	h = toHex $ SHA256.hash cnt
	BS.writeFile (dp ++ "/pre") h
	BS.writeFile (dp ++ "/" ++ convert '/' '_' fp) $ f cnt
--	print h
	lock l
	now <- toHex . SHA256.hash <$> BS.readFile fp
	if now == h
		then do	renameFile (dp ++ "/" ++ convert '/' '_' fp) fp
			unlock l
			removeFile $ dp ++ "/pre"
		else do unlock l
			threadDelay 1000000
			updateFile l dp fp f
	return ()

toHex :: BS.ByteString -> BS.ByteString
toHex = BS.concatMap $ BSC.pack . flip showHex ""

{-# LANGUAGE OverloadedStrings #-}

module Tools (
	makePage,
	setHomepageID,
	readBinaryFile,
	getPostData
	) where

import Control.Monad (liftM)
import System.IO (IOMode(..), openBinaryFile, hGetContents)
import System.FilePath (splitPath, dropTrailingPathSeparator)
import Data.Time
import System.Posix.User (
	getUserEntryForName, getGroupEntryForName,
	userID, groupID, setUserID, setGroupID)

import Data.HandleLike (HandleLike, HandleMonad)
import Data.Pipe (runPipe, (=$=))
import Data.Pipe.List (toList)
import Network.TigHTTP.Types (Request(..), Path(..), Post(..))

import qualified Data.ByteString.Char8 as BSC

makePage :: FilePath -> UTCTime -> String -> String -> String
makePage fp mt ttl bdy = "<!DOCTYPE html><html lang=\"UTF-8\"><head>"
	++ "<meta charset=\"UTF-8\"><title>" ++ ttl ++ "</title>"
	++ "<link href=\"/css/basic.css\" rel=\"stylesheet\" type=\"text/css\">"
	++ "</head><body>"
	++ "<small>"
	++ (if fp /= "/" then "<a href=\"/\">top</a> > " else "")
	++ concat (tail' $ map (uncurry pathToHref) (init $ getPaths fp))
	++ (last' . map fst $ getPaths fp)
	++ "<div align=\"right\"><small>更新日: <time>"
		++ show (utcToZonedTime japan mt)
		++ "</time></small></div>"
	++ "<div align=\"right\"><small>文責: 重城良国</small></div>"
	++ "<h1>" ++ ttl ++ "</h1>"
	++ filter (/= '\n') bdy
	++ "</body></html>"

japan :: TimeZone
japan = TimeZone 540 False "JST"

pathToHref :: String -> FilePath -> String
pathToHref n p = "<a href=\"" ++ p ++ "\">" ++ n ++ "</a> > "

getPaths :: FilePath -> [(String, FilePath)]
getPaths fp = zip (map dropTrailingPathSeparator ps) (tail' $ scanl (++) "" ps)
	where
	ps = splitPath fp

tail' :: [a] -> [a]
tail' [] = []
tail' (_ : t) = t

last' :: [String] -> String
last' [] = ""
last' [_] = "top"
last' xs = last xs

setHomepageID :: IO ()
setHomepageID = do
	getGroupEntryForName "homepage" >>= setGroupID . groupID
	getUserEntryForName "homepage" >>= setUserID . userID

readBinaryFile :: FilePath -> IO String
readBinaryFile path = openBinaryFile path ReadMode >>= hGetContents

getPostData :: HandleLike h => Request h -> HandleMonad h (Maybe BSC.ByteString)
getPostData (RequestPost _ _ Post { postBody = pb }) =
	liftM (fmap BSC.concat) . runPipe $ pb =$= toList
getPostData (RequestGet (Path p) _ _)
	| BSC.null rtn = return Nothing
	| otherwise = return . Just $ myTail rtn
	where
	rtn = BSC.dropWhile (/= '?') p
	myTail "" = ""
	myTail bs = BSC.tail bs
getPostData _ = return Nothing

{-# LANGUAGE OverloadedStrings #-}

module Tools (
	readBinaryFile,
	getPostData,

	isHtml,
	contentType,
	isBinary,
	addIndex,
	addPathSeparator,
	) where

import Control.Monad (liftM)
import System.IO (IOMode(..), openBinaryFile, hGetContents)
import System.FilePath -- (splitPath, dropTrailingPathSeparator)

import Data.HandleLike (HandleLike, HandleMonad)
import Data.Pipe (runPipe, (=$=))
import Data.Pipe.List (toList)
import Network.TigHTTP.Types -- (Request(..), Path(..), Post(..))

import qualified Data.ByteString.Char8 as BSC

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

ico, png, jpg, svg, css, html, plain, octet :: ContentType
ico = ContentType (TypeRaw "image") (SubtypeRaw "vnd.microsoft.icon") []
png = ContentType (TypeRaw "image") (SubtypeRaw "png") []
jpg = ContentType (TypeRaw "image") (SubtypeRaw "jpg") []
svg = ContentType (TypeRaw "image") (SubtypeRaw "svg") []
css = ContentType Text Css []
html = ContentType Text Html []
plain = ContentType Text Plain []
octet = ContentType (TypeRaw "application") (SubtypeRaw "octet-stream") []

contentType :: FilePath -> ContentType
contentType fp = case takeExtension fp of
	".ico" -> ico; ".png" -> png; ".jpg" -> jpg; ".svg" -> svg
	".css" -> css; ".html" -> html; ".hs" -> plain
	_ -> octet

isBinary :: ContentType -> Bool
isBinary = (`elem` [ico, png, jpg, octet])

isHtml :: ContentType -> Bool
isHtml = (== html)

addIndex, addPathSeparator :: FilePath -> FilePath
addIndex p
	| null $ takeBaseName p = p </> "index.html"
	| otherwise = p
addPathSeparator p
	| null $ takeExtension p = addTrailingPathSeparator p
	| otherwise = p
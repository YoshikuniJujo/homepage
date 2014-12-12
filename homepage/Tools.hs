{-# LANGUAGE OverloadedStrings #-}

module Tools (addIndex, addSep, contentType, isHtml, isBinary, getPostData) where

import Control.Monad (liftM)
import Data.HandleLike (HandleLike, HandleMonad)
import Data.Pipe (runPipe, (=$=))
import Data.Pipe.List (toList)
import System.FilePath (
	takeBaseName, takeExtension, (</>), addTrailingPathSeparator )
import Network.TigHTTP.Types (
	Request(..), Path(..), Post(..), ContentType(..), Type(..), Subtype(..) )

import qualified Data.ByteString.Char8 as BSC

addIndex, addSep :: FilePath -> FilePath
addIndex p | null $ takeBaseName p = p </> "index.html" | otherwise = p
addSep p | null $ takeExtension p = addTrailingPathSeparator p | otherwise = p

contentType :: FilePath -> ContentType
contentType fp = case takeExtension fp of
	".ico" -> ico; ".png" -> png; ".jpg" -> jpg; ".svg" -> svg
	".css" -> css; ".html" -> html; ".hs" -> plain; ".cabal" -> plain
	".ebuild" -> plain
	".gz" -> gz; ".tar.gz" -> targz
	_ -> octet

isHtml, isBinary :: ContentType -> Bool
isHtml = (== html)
isBinary = (`elem` [ico, png, jpg, gz, octet])

ico, png, jpg, svg, css, html, plain, gz, targz, octet :: ContentType
ico = ContentType (TypeRaw "image") (SubtypeRaw "vnd.microsoft.icon") []
png = ContentType (TypeRaw "image") (SubtypeRaw "png") []
jpg = ContentType (TypeRaw "image") (SubtypeRaw "jpg") []
svg = ContentType (TypeRaw "image") (SubtypeRaw "svg") []
css = ContentType Text Css []
html = ContentType Text Html []
plain = ContentType Text Plain []
gz = ContentType (TypeRaw "application") (SubtypeRaw "gzip") []
targz = ContentType (TypeRaw "application") (SubtypeRaw "x-tar-gz") []
octet = ContentType (TypeRaw "application") (SubtypeRaw "octet-stream") []

getPostData :: HandleLike h => Request h -> HandleMonad h (Maybe BSC.ByteString)
getPostData (RequestPost _ _ Post { postBody = pb }) =
	liftM (fmap BSC.concat) . runPipe $ pb =$= toList
getPostData (RequestGet (Path p) _ _) = return $ case BSC.dropWhile (/= '?') p of
	"" -> Nothing
	dat -> Just $ BSC.tail dat
getPostData _ = return Nothing

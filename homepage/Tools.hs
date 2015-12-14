{-# LANGUAGE OverloadedStrings #-}

module Tools (
	addIndex, addSep, contentType, isHtml, isBinary, getPostData, html) where

import Control.Monad (liftM)
import Data.Char (isLower, isUpper)
import Data.HandleLike (HandleLike, HandleMonad)
import Data.Pipe (runPipe, (=$=))
import Data.Pipe.List (toList)
import System.FilePath (
	takeFileName, takeBaseName, takeExtension, (</>), addTrailingPathSeparator )
import Network.TigHTTP.Types (
	Request(..), Path(..), Post(..), ContentType(..), Type(..), Subtype(..) )

import qualified Data.ByteString.Char8 as BSC

addIndex, addSep :: FilePath -> FilePath
addIndex p | null $ takeBaseName p = p </> "index.html" | otherwise = p
addSep p
	| null (takeExtension p) && any isLower (takeFileName p) =
		addTrailingPathSeparator p
	| otherwise = p

contentType :: FilePath -> ContentType
contentType fp | all isUpper $ takeFileName fp = plain
contentType fp = case takeExtension fp of
	".ico" -> ico; ".png" -> png; ".jpg" -> jpg; ".svg" -> svg
	".css" -> css; ".html" -> html;
	".hs" -> plain; ".hsc" -> plain; ".cabal" -> plain
	".scm" -> plain; ".coffee" -> plain
	".ebuild" -> plain; ".txt" -> plain; ".mf" -> plain; ".sf" -> plain
	".nml" -> plain; ".dat" -> plain; ".h" -> plain; ".c" -> plain
	".md" -> plain; ".asc" -> plain
	".gz" -> gz; ".tar.gz" -> targz; ".xpi" -> xpi; ".js" -> js
	_ -> octet

isHtml, isBinary :: ContentType -> Bool
isHtml = (== html)
isBinary = (`elem` [ico, png, jpg, gz, octet, xpi])

ico, png, jpg, svg, css, html, plain, gz, targz, xpi, octet :: ContentType
ico = ContentType (TypeRaw "image") (SubtypeRaw "vnd.microsoft.icon") []
png = ContentType (TypeRaw "image") (SubtypeRaw "png") []
jpg = ContentType (TypeRaw "image") (SubtypeRaw "jpg") []
svg = ContentType (TypeRaw "image") (SubtypeRaw "svg+xml") []
css = ContentType Text Css []
html = ContentType Text Html []
plain = ContentType Text Plain []
js = ContentType Text (SubtypeRaw "javascript") []
gz = ContentType (TypeRaw "application") (SubtypeRaw "gzip") []
targz = ContentType (TypeRaw "application") (SubtypeRaw "x-tar-gz") []
xpi = ContentType (TypeRaw "application") (SubtypeRaw "x-xpinstall") []
octet = ContentType (TypeRaw "application") (SubtypeRaw "octet-stream") []

getPostData :: HandleLike h => Request h -> HandleMonad h (Maybe BSC.ByteString)
getPostData (RequestPost _ _ Post { postBody = pb }) =
	liftM (fmap BSC.concat) . runPipe $ pb =$= toList
getPostData (RequestGet (Path p) _ _) = return $ case BSC.dropWhile (/= '?') p of
	"" -> Nothing
	dat -> Just $ BSC.tail dat
getPostData _ = return Nothing

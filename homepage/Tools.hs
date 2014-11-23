module Tools (
	processIndex,
	makePage
	) where

import System.FilePath
import Data.Time

processIndex :: FilePath -> FilePath
processIndex fp_ = if null $ takeBaseName fp then fp </> "index.html" else fp
	where
	fp = if null $ takeExtension fp_ then addTrailingPathSeparator fp_ else fp_

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

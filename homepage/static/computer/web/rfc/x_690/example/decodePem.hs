import Data.PEM
import System.Environment

import qualified Data.ByteString as BS

main :: IO ()
main = do
	"-in" : fpin : "-out" : fpout : _ <- getArgs
	cnt <- BS.readFile fpin
	let Right [pem] = pemParseBS cnt
	BS.writeFile fpout $ pemContent pem

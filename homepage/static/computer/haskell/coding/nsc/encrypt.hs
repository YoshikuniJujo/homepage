import System.Environment

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Nsc

main :: IO ()
main = do
	ps : _ <- getArgs
	BS.getContents
		>>= getStdRandom . encrypt (makeKey $ BSC.pack ps)
		>>= BS.putStr

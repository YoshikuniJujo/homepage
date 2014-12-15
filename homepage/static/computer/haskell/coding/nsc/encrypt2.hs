import Control.Applicative
import System.Environment

import qualified Data.ByteString as BS
import qualified Data.ByteString.Char8 as BSC

import Nsc

main :: IO ()
main = do
	ps : sd : _ <- getArgs
	flip (encrypt . makeKey $ BSC.pack ps)
		(mkStdGen $ read sd) <$> BS.getContents
			>>= BS.putStr . fst

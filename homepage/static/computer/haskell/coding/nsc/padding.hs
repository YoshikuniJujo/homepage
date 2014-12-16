import Control.Applicative

import qualified Data.ByteString as BS

padToLen :: BS.ByteString -> Int -> BS.ByteString
padToLen s n = s `BS.append` BS.replicate
	(n - BS.length s)
	(fromIntegral $ n - BS.length s - 1)

blkSzToLen :: Int -> BS.ByteString -> Int
blkSzToLen b s = (BS.length s `div` b + 1) * b

padding :: Int -> BS.ByteString -> BS.ByteString
padding b = ($) <$> padToLen <*> blkSzToLen b

unpadding :: BS.ByteString -> BS.ByteString
unpadding p = BS.take
	(BS.length p - fromIntegral (BS.last p) - 1) p

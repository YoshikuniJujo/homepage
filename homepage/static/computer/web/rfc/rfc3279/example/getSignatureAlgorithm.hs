import Data.PEM
import Data.ASN1.Encoding
import Data.ASN1.BinaryEncoding
import System.Environment

import qualified Data.ByteString as BS
import qualified Data.ByteString.Lazy as LBS

import Asn1Container

main :: IO ()
main = do
	fp : _ <- getArgs
	cnt <- BS.readFile fp
	let Right pem = pemParseBS cnt
	print . either undefined
			((!! 1) . fromSequence . head . fst . parseAsn1Container)
		. decodeASN1 BER
		. LBS.fromStrict . pemContent $ head pem

fromSequence :: Asn1Container -> [Asn1Container]
fromSequence (CntSequence as) = as

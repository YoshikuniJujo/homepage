module DecodeAsn1Common (
	runAnalyzer, decodeTag1, decodeTag) where

import Control.Applicative
import Data.Bits
import Data.Word
import qualified Data.ByteString as BS
import Analyzer

data Asn1Tag
	= Asn1Tag TagClass DataClass Integer
	deriving Show

data TagClass
	= Universal
	| Application
	| ContextSpecific
	| Private
	deriving Show

data DataClass
	= Primitive
	| Constructed
	deriving Show

decodeTag :: Analyzer BS.ByteString Asn1Tag
decodeTag = decodeTag1 >>= \(tc, dc, mtn) -> maybe
	(Asn1Tag tc dc <$> decodeTagR 0)
	(return . Asn1Tag tc dc . fromIntegral)
	mtn

decodeTag1 :: Analyzer BS.ByteString
	(TagClass, DataClass, Maybe Word8)
decodeTag1 = flip fmap token $ \w -> let
	tc = case w `shiftR` 6 of
		0 -> Universal
		1 -> Application
		2 -> ContextSpecific
		3 -> Private
		_ -> error "never occur"
	dc = if testBit w 5
		then Constructed
		else Primitive
	tn = case w .&. 0x1f of
		0x1f -> Nothing
		n	| n < 0x1f -> Just n
			| otherwise ->
				error "never occur" in
	(tc, dc, tn)

decodeTagR :: Integer -> Analyzer BS.ByteString Integer
decodeTagR n = token >>= \w -> if testBit w 7
	then decodeTagR $
		n `shiftL` 7 .|. fromIntegral (w .&. 0x7f)
	else return $ n `shiftL` 7 .|.  fromIntegral w

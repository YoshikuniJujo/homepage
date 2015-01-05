{-# LANGUAGE TypeFamilies #-}

module DecodeAsn1Common (
	runAnalyzer, decodeTag1, decodeTag) where

import Control.Applicative
import Control.Monad
import Data.Bits
import Data.Word8
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
	(Asn1Tag tc dc <$> decodeTagR0)
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

decodeTagR0 :: Analyzer BS.ByteString Integer
decodeTagR0 = decodeTagR 0 >>= \n -> do
	when (n <= 30) $ fail
		"Use single byte for tag number 0 - 30"
	return n

decodeTagR :: Integer -> Analyzer BS.ByteString Integer
decodeTagR 0 = token >>= \w -> do
	when (w == 0x80) $ fail
		"Redundant byte for tag number"
	if testBit w 7
		then decodeTagR $ fromIntegral (w .&. 0x7f)
		else return $ fromIntegral w
decodeTagR n = token >>= \w -> if testBit w 7
	then decodeTagR $
		n `shiftL` 7 .|.  fromIntegral (w .&. 0x7f)
	else return $ n `shiftL` 7 .|.  fromIntegral w

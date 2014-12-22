module Asn1Container (
	Asn1Container(..),
	parseAsn1Container
	) where

import Control.Arrow
import Data.ASN1.Types

data Asn1Container
	= CntAtom ASN1
	| CntSequence [Asn1Container]
	| CntSet [Asn1Container]
	| CntContext Int [Asn1Container]
	deriving Show

parseAsn1Container :: [ASN1] -> ([Asn1Container], [ASN1])
parseAsn1Container [] = ([], [])
parseAsn1Container (End Sequence : r) = ([], r)
parseAsn1Container (Start Sequence : as) = let
	(s, r) = parseAsn1Container as in
	first (CntSequence s :) $ parseAsn1Container r
parseAsn1Container (End Set : r) = ([], r)
parseAsn1Container (Start Set : as) = let
	(s, r) = parseAsn1Container as in
	first (CntSet s :) $ parseAsn1Container r
parseAsn1Container (End (Container Context _) : r) = ([], r)
parseAsn1Container (Start (Container Context n) : as) = let
	(s, r) = parseAsn1Container as in
	first (CntContext n s :) $ parseAsn1Container r
parseAsn1Container (a : as) = first (CntAtom a :) $ parseAsn1Container as

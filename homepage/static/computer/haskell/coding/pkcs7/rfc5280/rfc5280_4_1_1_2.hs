import Asn1Container
import Data.ASN1.Types

data AlgorithmIdentifier = AlgorithmIdentifier HashId CryptoId
	deriving Show

data HashId = Md2 | Md5 | Sha1 deriving Show

data CryptoId = Rsa | Dsa | Ecdsa deriving Show

algIdToAsn1c :: AlgorithmIdentifier -> Maybe Asn1Container
algIdToAsn1c (AlgorithmIdentifier Md2 Rsa) = Just $
	CntSequence [CntAtom $ OID [1, 2, 840, 113549, 1, 1, 2], CntAtom Null]
algIdToAsn1c (AlgorithmIdentifier Md5 Rsa) = Just $
	CntSequence [CntAtom $ OID [1, 2, 840, 113549, 1, 1, 4], CntAtom Null]
algIdToAsn1c (AlgorithmIdentifier Sha1 Rsa) = Just $
	CntSequence [CntAtom $ OID [1, 2, 840, 113549, 1, 1, 5], CntAtom Null]
algIdToAsn1c (AlgorithmIdentifier Sha1 Dsa) = Just $
	CntSequence [CntAtom $ OID [1, 2, 840, 10040, 4, 3]]
algIdToAsn1c (AlgorithmIdentifier Sha1 Ecdsa) = Just $
	CntSequence [CntAtom $ OID [1, 2, 840, 10045, 4, 1]]
algIdToAsn1c _ = Nothing

asn1cToAlgId :: Asn1Container -> Maybe AlgorithmIdentifier
asn1cToAlgId (CntSequence
	[CntAtom (OID [1, 2, 840, 113549, 1, 1, 2]), CntAtom Null]) =
	Just $ AlgorithmIdentifier Md2 Rsa
asn1cToAlgId (CntSequence
	[CntAtom (OID [1, 2, 840, 113549, 1, 1, 4]), CntAtom Null]) =
	Just $ AlgorithmIdentifier Md5 Rsa
asn1cToAlgId (CntSequence
	[CntAtom (OID [1, 2, 840, 113549, 1, 1, 5]), CntAtom Null]) =
	Just $ AlgorithmIdentifier Sha1 Rsa
asn1cToAlgId (CntSequence
	[CntAtom (OID [1, 2, 840, 10040, 4, 3])]) =
	Just $ AlgorithmIdentifier Sha1 Dsa
asn1cToAlgId (CntSequence
	[CntAtom (OID [1, 2, 840, 10045, 4, 1])]) =
	Just $ AlgorithmIdentifier Sha1 Ecdsa
asn1cToAlgId _ = Nothing

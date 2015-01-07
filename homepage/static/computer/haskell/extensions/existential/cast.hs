import Data.Maybe
import Data.Typeable
import Unsafe.Coerce

cast' :: (Typeable a, Typeable b) => a -> Maybe b
cast' x = r
	where
	r = if typeOf x == typeOf (fromJust r)
		then Just $ unsafeCoerce x
		else Nothing

import Data.List

zipRaw, zipF, zipU :: [a] -> [b] -> [(a, b)]
zipRaw (x : xs) (y : ys) = (x, y) : zipRaw xs ys
zipRaw _ _ = []

zipF = foldr s (const [])
	where
	s x f (y : ys) = (x, y) : f ys
	s _ _ _ = []

zipU = curry . unfoldr $ \l -> case l of
	(x : xs, y : ys) -> Just ((x, y), (xs, ys))
	_ -> Nothing

zipWithRaw :: (a -> b -> c) -> [a] -> [b] -> [c]
zipWithRaw op (x : xs) (y : ys) = x `op` y : zipWithRaw op xs ys
zipWithRaw _ _ _ = []

zipWithF op = foldr s (const [])
	where
	s x f (y : ys) = x `op` y : f ys
	s _ _ _ = []

zipWithU op = curry . unfoldr $ \l -> case l of
	(x : xs, y : ys) -> Just (x `op` y, (xs, ys))
	_ -> Nothing

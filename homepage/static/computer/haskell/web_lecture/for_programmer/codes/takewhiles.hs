import Data.List

takeWhileRaw, takeWhileF :: (a -> Bool) -> [a] -> [a]
takeWhileRaw p (x : xs) | p x = x : takeWhileRaw p xs
takeWhileRaw _ _ = []

takeWhileF p = foldr (\x -> if p x then (x :) else const []) []

takeWhileU p = unfoldr $ \l -> case l of
	x : xs | p x -> Just (x, xs)
	_ -> Nothing

dropWhileRaw, dropWhileF :: (a -> Bool) -> [a] -> [a]
dropWhileRaw p (x : xs) | p x = dropWhileRaw p xs
dropWhileRaw _ xs = xs

dropWhileF p xs = foldr s (const []) xs True where
	s x f True
		| p x = f True
		| otherwise = x : f False
	s x f False = x : f False

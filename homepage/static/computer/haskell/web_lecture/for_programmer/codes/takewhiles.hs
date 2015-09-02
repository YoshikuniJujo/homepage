import Data.List

takeWhileRaw, takeWhileF :: (a -> Bool) -> [a] -> [a]
takeWhileRaw p (x : xs) | p x = x : takeWhileRaw p xs
takeWhileRaw _ _ = []

takeWhileF p = foldr (\x -> if p x then (x :) else const []) []

takeWhileU p = unfoldr $ \l -> case l of
	x : xs | p x -> Just (x, xs)
	_ -> Nothing

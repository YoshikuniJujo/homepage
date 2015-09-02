import Data.List

takeRaw, takeF, takeU :: Int -> [a] -> [a]
takeRaw n (x : xs) | n > 0 = x : takeRaw (n - 1) xs
takeRaw _ _ = []

takeF = flip $ foldr s (const [])
	where s x f n | n > 0 = x : f (n - 1); s _ _ _ = []

takeU = curry . unfoldr $ \nl -> case nl of
	(n, x : xs) | n > 0 -> Just (x, (n - 1, xs))
	_ -> Nothing

dropRaw, dropF :: Int -> [a] -> [a]
dropRaw n (x : xs) | n > 0 = dropRaw (n - 1) xs
dropRaw _ xs = xs

dropF = flip $ foldr s (const [])
	where s _ f n | n > 0 = f $ n - 1; s x f _ = x : f 0

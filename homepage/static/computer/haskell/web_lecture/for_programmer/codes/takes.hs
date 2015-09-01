import Data.List

takeRaw :: Int -> [a] -> [a]
takeRaw n (x : xs) | n > 0 = x : takeRaw (n - 1) xs
takeRaw _ _ = []

takeU = curry . unfoldr $ \nl -> case nl of
	(n, x : xs) | n > 0 -> Just (x, (n - 1, xs))
	_ -> Nothing

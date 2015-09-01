import Control.Arrow

partitionRaw, partitionRaw' :: (a -> Bool) -> [a] -> ([a], [a])
partitionRaw p (x : xs)
	| p x = (x : ts, es)
	| otherwise = (ts, x : es)
	where (ts, es) = partitionRaw p xs
partitionRaw _ _ = ([], [])

partitionRaw' p (x : xs) =
	(if p x then first else second) (x :) $ partitionRaw' p xs
partitionRaw' _ _ = ([], [])

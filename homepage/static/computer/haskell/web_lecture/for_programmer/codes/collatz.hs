takeTo _ [] = []
takeTo p (x  : xs)
	| p x = [x]
	| otherwise = x : takeTo p xs

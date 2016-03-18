root :: Double -> [Double]
root 0 = [0]
root x	| x < 0 = []
	| otherwise = [- sqrt x, sqrt x]

-- root (root a + b)

calc :: Double -> Double -> [Double]
calc a b = root a >>= root . (+ b)

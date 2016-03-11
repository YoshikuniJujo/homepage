myAdd :: (a -> Int) -> Int -> (a -> Int)
myAdd f y = \x -> f x + y

addArg2 :: (b -> c -> d) -> ((a -> b) -> c -> (a -> d))
addArg2 fun f y x = fun (f x) y

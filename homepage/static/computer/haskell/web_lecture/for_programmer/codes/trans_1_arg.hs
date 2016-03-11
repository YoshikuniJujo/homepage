import Data.Char (chr, ord)

myChr :: (a -> Int) -> (a -> Char)
myChr f = \x -> chr $ f x

addArg :: (b -> c) -> (a -> b) -> a -> c
addArg fun f x = fun (f x)

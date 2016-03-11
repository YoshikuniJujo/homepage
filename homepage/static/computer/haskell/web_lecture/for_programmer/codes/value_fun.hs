valToFun :: a -> ((a -> b) -> b)
valToFun x = ($ x)

funToVal :: ((a -> b) -> b) -> a
funToVal = ($ id)

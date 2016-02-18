data Size = Short | Tall | Grande | Venti

instance Eq Size where
	Short == Short = True
	Tall == Tall = True
	Grande == Grande = True
	Venti == Venti = True
	_ == _ = False

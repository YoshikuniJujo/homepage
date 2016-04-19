table :: [(Char, Char)]
table = zip ['a' .. 'z'] (['h' .. 'z'] ++ ['a' .. 'g'])

crypt :: String -> Maybe String
crypt = mapM (`lookup` table)

decrypt :: String -> Maybe String
decrypt = mapM (`lookup` map (uncurry $ flip (,)) table)

myMapM :: Applicative m => (a -> m b) -> [a] -> m [b]
myMapM f (x : xs) = (:) <$> f x <*> myMapM f xs
myMapM f _ = pure []

crypt_ :: String -> Maybe String
crypt_ (c : cs) = (:) <$> lookup c table <*> crypt_ cs
crypt_ _ = Just ""

decrypt_ :: String -> Maybe String
decrypt_ (c : cs) = (:) <$> lookup c (map (uncurry $ flip (,)) table) <*> decrypt_ cs
decrypt_ _ = Just ""

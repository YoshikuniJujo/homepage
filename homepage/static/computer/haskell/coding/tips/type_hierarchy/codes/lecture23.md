第23回 型の階層
===============

はじめに
--------

* Haskellで型の階層を作る方法を見ていこう
* 前回学んだ存在型とTypeableのcastを使う
* やや複雑だが非常に巧妙だ
* オブジェクト指向プログラミングへの回帰
* オブジェクト指向的に「どうしても」書きたいときに使う
* upcastやdowncastができる
* castできない型への変換は明示的にNothingになる
* 動きとしては以下のようになる
	+ 最上位の型への変換
	+ 最上位の型からの変換

はじめに
--------

* オブジェクト指向的な例を挙げるところまではやらない
* 型の階層を作り、階層間でcastできるところまでを示す
* 以下の拡張機能が必要
	+ ExistentialQuantification
	+ DeriveDataTypeable

生物の例
--------

* 生物型を最上位の型とする
* 生物型の下に動物型と植物型があり
* 動物型の下に犬型と猫型があり
* 植物型の下に樹型と草型がある
* また生物型の直下に細菌型も作ろう

* そういう例を作っていこう

SomeLife
--------

* まずは最上位の型を作成する
	data SomeLife = forall l . Life l => SomeLife l
		deriving Typeable
* すべての生物型はこの型に変換できる
* この型から他の生物型への変換が可能
* Showクラスのインスタンスにしておく
    instance Show SomeLife where
	showsPrec d (SomeLife l) = showParen (d > 10) $
		showString "SomeLife " . showsPrec 11 l

Lifeクラス
----------

* すべての生物はSomeLifeと相互変換できる必要がある
* その条件を満たすLifeクラスを作成する
	class (Typeable l, Show l) => Life l where
		toLife :: l -> SomeLife
		fromLife :: SomeLife -> Maybe l
* SomeLifeへの変換は必ず成功する
* SomeLifeからの変換は失敗する可能性がある

クラス関数のデフォルト値
------------------------

* toLifeとfromLifeにはデフォルト値を設定しておく
	toLife = SomeLife
	fromLife (SomeLife l) = cast l

細菌型
------

* 細菌型を以下のように定義する
	data Bacteria = Bacteria deriving (Typeable, Show)
* Lifeクラスのインスタンスにする
	instance Life Bacteria
* toLifeもfromLifeもデフォルト値。where以下は不要
* 試してみる
	> toLife Bacteria
	SomeLife Bacteria
	> fromLife it :: Maybe Bacteria
	Just Bacteria

動物型
------

* 動物型を作る
* この型は他の型をまとめる上位の型となる
	data Animal = forall a . Life a => Animal a
		deriving Typeable
* Lifeクラスのインスタンスにする
	instance Life Animal
* Showクラスのインスタンスにする
    instance Show Animal where
	showPrec d (Animal a) = showParen (d > 10) $
		showString "Animal " . showsPrec 11 a

動物型
------

* ここがポイント
* Animalの下位型用のtoLife, fromLifeを定義する
	animalToLife :: Life a => a -> SomeLife
	animalToLife = toLife . Animal
	animalFromLife :: Life a => SomeLife -> Maybe a
	animalFromLife l = do
		Animal a <- fromLife l
		cast a
* つまり、SomeLife (Animal ...)のような構造

犬型
----

* 犬型を定義する
	newtype Dog = Dog String deriving (Typeable, Show)
* Lifeクラスのインスタンスにする
	instance Life Dog where
		toLife = animalToLife
		fromLife = animalFromLife

犬型
----

* 試してみる
	> toLife $ Dog "pochi"
	SomeLife (Animal (Dog "pochi"))
	> let dog = it
	> fromLife dog :: Maybe Dog
	Just (Dog "pochi")
	> fromLife dog :: Maybe Animal
	Just (Animal (Dog "pochi"))
	> fromLife dog :: Maybe Bacteria
	Nothing

猫型
----

* 猫型を定義
	newtype Cat = Cat String deriving (Typeable, Show)
	instance Life Cat where
		toLife = animalToLife
		fromLife = animalFromLife

植物型
------

* 植物型を定義する
data Plant = forall p . Life p => Plant p
	deriving Typeable

instance Show Plant where
	showsPrec d (Plant p) = showParen (d > 10) $
		showString "Plant " . showsPrec 11 p

instance Life Plant

植物型
------

* 植物用のtoLife, fromLife
	plantToLife :: Life p => p -> SomeLife
	plantToLife = toLife . Plant

	plantFromLife :: Life p => SomeLife -> Maybe p
	plantFromLife l = do
		Plant p <- fromLife l
		cast p

樹型
----

* 樹型
	newtype Tree = Tree String
		deriving (Typeable, Show)

	instance Life Tree where
		toLife = plantToLife
		fromLife = plantFromLife

草型
----

* 草型
	data Grass = Grass String deriving (Typeable, Show)

	instance Life Grass where
		toLife = plantToLife
		fromLife = plantFromLife

人間型
------

* ここで、動物型の下位型となる人間型を定義する
* 人間型自体も上位型とする
* 新しい点はない
	上位型と下位型の性質の両方を持つ

人間型
------

* 人間型を定義する
	data Human = forall h . Life h => Human h
		deriving Typeable
* Lifeクラスのインスタンスにする
	instance Life Human where
		toLife = animalToLife
		fromLife = animalFromLife
* Showクラスのインスタンスにする
instance Show Human where
	showsPrec d (Human h) = showParen (d > 10) $
		showString "Human " . showsPrec 11 h

人間型
------

* 人間用のtoLife, fromLifeを定義する
	humanToLife :: Life h => h -> SomeLife
	humanToLife = toLife . Human

	humanFromLife :: Life h => SomeLife -> Maybe h
	humanFromLife l = do
		Human h <- fromLife l
		cast h

プログラマ型
------------

* プログラマ型
	newtype Programmer = Programmer String
		deriving (Typeable, Show)

	instance Life Programmer where
		toLife = humanToLife
		fromLife = humanFromLife

作家型
------

* 作家型
	newtype Author = Author String
		deriving (Typeable, Show)

	instance Life Author where
		toLife = humanToLife
		fromLife = humanFromLife

生物型
------

* 生物型自体はまだLifeクラスのインスタンスではない
* これもLifeクラスのインスタンスにしておくと便利
	instance Life SomeLife where
		toLife = id
		fromLife = Just

castLife
--------

* Lifeクラスのインスタンス間での型キャスト
	castLife :: (Life l1, Life l2) => l1 -> Maybe l2
	castLife = fromLife . toLife

castLife
--------

* 試してみる
> castLife $ Programmer "Matz" :: Maybe Human
Just (Human (Programmer "Matz"))
> castLife $ Author "Souseki" :: Maybe SomeLife
Just (SomeLife (Animal (Human (Author "Souseki"))))
> let Just souseki = it
> :t souseki
souseki :: SomeLife
> castLife souseki :: Maybe Human
Just (Human (Author "Souseki"))
> castLife souseki :: Maybe Plant
Nothing

filterLife
----------

* 面白い関数が作れる
	filterLife :: (Life l1, Life l2 => [l1] -> [l2]
	filterLife = catMaybe . map castLife
* この関数は例えば以下のように使う
	filterLife lives :: [Human]
	+ livesのなかから人間型に属する型のみを抽出する

withLifeIO
----------

* 面白い関数が作れる
	withLifeIO :: (Life l1, Life l2) =>
		l1 -> (l2 -> IO ()) -> IO ()
	withLifeIO l f = maybe (return ()) f $ castLife l
* 特定のタイプに属する値にのみ関数を適用する
	withLifeIO (Author "souseki" $ \\(h :: Human) ->
		print h
	arrowIText t 2 "Human (Author "souseki")
	withLifeIO Bacteria $ \\(h :: Human) -> print h
	arrowIText t 2 "何もしない

まとめ
------

* 型の階層構造を作ってみた
* 階層内での型キャストが可能
* キャストできない場合はNothingが返る
	arrowIText t 1 "安全
* 特定の範囲の型のみを抽出したり
* 特定の範囲の型のみにIOアクションを実行したりできる
* 次回の例外処理にこの仕組みが使われている

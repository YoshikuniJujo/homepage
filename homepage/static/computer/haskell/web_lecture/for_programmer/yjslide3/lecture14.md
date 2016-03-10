第14回 ファンクター
===================

はじめに
--------

* 多相型に対する型クラスを作ることができる
* 型変数をひとつとる多相型はコンテナと考えることができる
	- コンテナとは値を格納するもの
	- Maybe型: 0または1個の値を格納する
	- リスト: 複数の値を順番に格納する
	- 木: 複数の値を木構造として格納する
* これらのコンテナの構造そのものの性質を表現できる
* Functorはそのような型クラスのひとつ
* 「中身に関数を適用できる」という性質を型クラスにした

リスト
------

* リストの中身の値に関数を適用する関数は
	map :: (a -> b) -> [a] -> [b]
* 試してみる
* lectures/lecture14ディレクトリを作ってそこに移動
	Prelude> map even [1 .. 5]
	itext t 1 $ show $ map even [1 :: Int .. 5]
 ]

Maybe
-----

* Maybe型の中身の値に関数を適用する関数を書いてみる
	appMaybe :: (a -> b) -> Maybe a -> Maybe b
	appMaybe _ Nothing = Nothing
	appMaybe f (Just x) = Just $ f x
* lecture/lecture14/functor.hsに書き込もう
* 試してみる
	Prelude> :load functor.hs
	*Main> appMaybe even Nothing
	itext t 1 $ show $ appMaybe (even :: Int -> Bool) Nothing, \t -> do
	*Main> appMaybe even (Just 4)
	itext t 1 $ show $ appMaybe even (Just 4 :: Maybe Int)

木
--

* 二分木を作ろう
	data BinTree a
	itext t 2 "= Node (BinTree a) (BinTree a)
	itext t 2 "| Leaf a
	itext t 2 "deriving Show
* 二分木のすべての葉に関数を適用する関数
	mapTree :: (a -> b) -> BinTree a -> BinTree b
	mapTree f (Leaf x) = Leaf $ f x
	mapTree f (Node tl tr) =
	itext t 2 "Node (mapTree f tl) (mapTree f tr)
* これをfunctor.hsに書き込もう

試してみる
----------

	text t "*Main> :reload
	text t "*Main> let t = Node (Leaf 3) (Node (Leaf 4) (Leaf 5))
	text t "*Main> mapTree even t
	itext t 1 $ show $ mapTree even tree, \t -> do
	text t "*Main> mapTree (* 3) t
	itext t 1 $ show $ mapTree (* 3) tree

それぞれのmap
-------------

* それぞれの型に対するmap的関数
	map :: (a -> b) -> [] a -> [] b
	appMaybe :: (a -> b) -> Maybe a -> Maybe b
	mapTree :: (a -> b) -> BinTree a -> BinTree b
* これらの型を比較してみると以下の形が得られる
	(a -> b) -> f a -> f b
* fにそれぞれの型を入れれば良い
* 型fの「中身に関数を適用可」という性質を言い換えると
	- 「(a -> b) -> f a -> f b型の関数を持つ」となる

Functor
-------

* 「中身に関数を適用可」を表すFunctorクラスがある
	class Functor f where
	itext t 2 "fmap :: (a -> b) -> f a -> f b
* リストやMaybeはFunctorクラスのインスタンス
	- リストに対するfmapはmapと同じ
	- Maybeに対するfmapはappMaybeと同じ
* 試してみよう
	*Main> fmap even [1 .. 5]
	itext t 1 $ show $ fmap even [1 :: Int .. 5], \t -> do
	*Main> fmap even $ Just 8
	itext t 1 $ show $ fmap even $ Just (8 :: Int)

木をFunctorにする
-----------------

* BinTreeは自作のデータ型なのでまだFunctorじゃない
* Functorにする
	instance Functor BinTree where
	itext t 2 "fmap = mapTree
* これをfunctor.hsに書き込もう
* 試してみよう
	text t "*Main> :reload
	text t "*Main> let t = Node (Leaf 3) (Node (Leaf 4) (Leaf 5))
	text t "*Main> fmap even t
	text t $ show $ fmap even tree

何がうれしいの?
---------------

* 値が何らかの構造のなかに存在する場合に
* その構造とは関係なく値に何かをしたい場合がある
* 例えば文字が入っている「何か」に対して
* その文字を「文字コード」に変えたいとする
* そのとき、その「何か」がFunctorであれば以下が使える
	toCode :: Functor f => f Char -> f Int
	toCode = fmap ord
* これをfunctor.hsに書き込もう
* Data.Charモジュールのordを使うので以下を先頭に書こう
	import Data.Char (ord)

試してみる
----------

	*Main> :reload
	*Main> toCode ['a', 'b', 'c']
		itext t (- 1) $ show $ toCode ['a', 'b', 'c']
	*Main> toCode $ Just 'd'
		itext t (- 1) $ show $ toCode $ Just 'd'
	*Main> let t = Node (Leaf 'e') (Node (Leaf 'f') (Leaf 'g'))
	*Main> toCode t
		itext t (- 1) $ show $ toCode $ Node (Leaf 'e') (Node (Leaf 'f') (Leaf 'g'))

* 様々なデータ構造においてその中身に同じ変換が行えた


まとめ
------

* 多相型に対する型クラスの例としてFunctorを取り上げた
* Functorは「中身に対して関数適用可」を表す型クラス
* 以下のクラス関数を持っている
	fmap :: (a -> b) -> f a -> f b
* これはfという構造中の型aの値に関数を適用する関数
* Functorによって抽象化された構造を使えば
	- コンテナの種類にかかわらず
	- 値に対する操作を定義することができる

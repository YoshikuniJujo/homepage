第15回 関数の変換
=================

はじめに
--------

* 単純な関数を高階関数に変えたり
* 引数を追加したり、減らしたり
* いろいろな変換を関数に対してすることができる
* 型と定義とを見くらべながらやっていく
* 次回「モナド」を学ぶときにこの操作を使うとわかりやすい
* 関数に対するいろいろな変換を見ていこう

(->)は右結合
------------

* 関数に対する変換を見ていくわけだが
* ひとつ頭に置いておくと良いことがある
* 型を作るときに使われる(->)は右結合である
* 以下のふたつの定義は同じこと
	a -> b -> c
	a -> (b -> c)
* 以下のふたつが同じということ
	- 引数を2つとる関数
	- 引数を1つとり「引数を1つとる関数」を返す関数

curryの2つの見方
---------------

* curryを例にして見てみよう
	curry :: ((a, b) -> c) -> (a -> b -> c)
	curry f = \\x y -> f (x, y)
* これは以下のように考えられる
	- タプルをとる関数を
	- 引数を2つとる関数に変換する関数
* 全く同じことを以下のようにも書ける
	curry :: ((a, b) -> c) -> a -> b -> c
	curry f x y = f (x, y)
* 同じことだが、この表記だと以下のように読める
	- タプルをとる関数の他に引数を2つとり
	- それらをタプルにし第一引数の関数を適用する関数

()
--

* ()という値をひとつだけ持つ()という型がある
* ()以外の値は持たない
* Bool型はFalseとTrueの2つの値を取るので
	- ()型はBool型以上に単純な型と言える
* この値はあってもなくても変わらない
* よって以下はどれも同じものと考えられる
	a
	() -> a
	(a, ())


()
--

* 以下の2つが同じであることを確認してみる
	val :: a
	fun :: () -> a
* 追加の情報なしに互いに定義できれば
	- それらは「同じ」であると言うことができる
* funをvalを使って定義してみる
	fun () = val
* valをfunを使って定義してみる
	val = fun ()
* 互いに定義できたのでfunとvalは同じものと考えられる

()
--

* 以下の2つについても確認しておこう
	val :: a
	tup :: (a, ())
* tupをvalを使って定義してみる
	tup = (val, ())
* valをtupを使って定義してみる
	val = fst tup
* よってaと(a, ())は同じである

()(まとめ)
----------

* Bool型はTrueかFalseのどちらかなので
	- どちらかがわかれば情報が増えることになる
* ()型には()しかないので
	- 「()だ!」と言っても情報は増えない
* よって()型は
	- 引数の位置に出てきても
	- タプルのなかに出てきても取り除くことができる
* 型同士が「同じ」ものであることを
	- 追加の情報なしに
	- 互いに相手を定義することで確かめられる

単純な値
--------

* たとえば以下の関数について考えよう
	eight :: (Int -> b) -> b
	eight f = f 8
* lectures/lecture15ディレクトリを作成し
* transFuns.hsに書き込もう
* eightは第一引数の関数に8を適用する関数
* しかし、これは単なる8とほとんど同じものである

単純な値
--------

* 「単純な値とほとんど同じものである」ことの確認
	% ghci transFuns.hs
	*Main> eight (+ 5)
		itext t 1 $ show $ eight (+ 5), \t -> do
	*Main> eight even
		itext t 1 $ show $ eight even, \t -> do
	*Main> eight id
		itext t 1 $ show $ eight id

単純な値
--------

* すべての値がeightと同じような形に変換できる
* もとの値をxとし型をaとすると
	x :: a
	funX :: (a -> b) -> b
	funX f = f x
* この変換を行う関数を作ってみよう
	valToFun :: a -> ((a -> b) -> b)
	valToFun x = \\f -> f x
* 同じことを以下のようにも書ける
	valToFun :: a -> (a -> b) -> b
	valToFun x f = f x
* どちらかの定義をtransFuns.hsに書き込もう

試してみる
----------

* 試してみよう
	*Main> :reload
	*Main> let three = valToFun 3
	*Main> three (+ 7)
		itext t 1 $ show $ three (+ 7), \t -> do
	*Main> three even
		itext t 1 $ show $ three even, \t -> do
	*Main> :load + Data.Char
	*Main Data.Char> let c = valToFun 'c'
	*Main Data.Char> c isLower
		itext t 1 $ show $ c isLower

単純な値
--------

* 単純な値から高階関数の形に変換する関数を作った
* 逆の方向の変換も可能である
* eight idで見たようにidをつけてやれば良い
	fun :: (a -> b) -> b
	val :: a
	val = fun id

単純な値(まとめ)
----------------

* 以下の2つの型は同じものと考えられる
	a
	(a -> b) -> b
* 前者を後者の形式に変換する関数valToFunを定義した
	valToFun :: a -> (a -> b) -> b
	valToFun x f = f x
* 逆方向の変換は関数形式のほうに引数としてidを与える
* 以下のような形の関数はより単純な値に置き換え可
	(Int -> b) -> b
	(Char -> b) -> b
	([Int] -> b) -> b
	((Int -> Char) -> b) -> b

一引数関数
----------

* 以下の関数を見てみよう
	myChr :: (a -> Int) -> (a -> Char)
	myChr f = \\x -> chr $ f x
* 上記と以下をtransFuns.hsに書き込もう
	import Data.Char (chr, ord)
* 以下のように読める
	- 関数fを引数にとり
	- 「引数にfを適用しchrを適用する関数」を返す

一引数関数
----------

* 使ってみる
	*Main> :reload
	*Main> myChr (* 2) 55
		itext t 1 $ show $ myChr (* 2) 55, \t -> do
	*Main> myChr ord 'j'
		itext t 1 $ show $ myChr ord 'j', \t -> do
	*Main> (chr . (* 2)) 55
		itext t 1 $ show $ (chr . (* 2)) 55, \t -> do
	*Main> (chr . ord) 'j'
		itext t 1 $ show $ (chr . ord) 'j'

一引数関数
----------

* myChrの定義は以下のように書き換えられる
	myChar :: (a -> Int) -> (a -> Char)
	myChar f = \\x -> chr $ f x
	arrowIText t 1 "myChar f = chr . f
	arrowIText t 1 "myChar = (chr .)
* myCharは関数chrと何かを合成する関数
* より一般的にすると
	fun :: b -> c
	fun' :: (a -> b) -> (a -> c)
	fun' = (fun .)

一引数関数
----------

* (b -> c)の形から(a -> b) -> (a -> c)の形にする
* 以下のように考えることもできる
	- 引数と返り値の両方に引数をひとつ追加する
* これを行う関数を作ろう
	addArg :: (b -> c) -> ((a -> b) -> (a -> c))
	addArg f = \\g -> (\\x -> f $ g x)
* これを変換していくと最終的には以下のようになる
	addArg :: (b -> c) -> (a -> b) -> (a -> c)
	addArg = (.)
* 単なる関数合成になる

一引数関数
----------

* fとgの合成関数f . gは「gを適用した結果にfを適用する」
* 二引数関数としてみると(.)は関数合成する関数
	(.) :: (b -> c) -> (a -> b) -> (a -> c)
* (.)を一引数関数としてみると
	(.) :: (b -> c) -> ((a -> b) -> (a -> c))
* 「引数と返り値の両方に引数を1つ追加する関数」と読める

一引数関数
----------

* 逆方向の変換をする関数を作成する
	rmArg :: ((a -> b) -> (a -> c)) -> (b -> c)
	rmArg f = \\x -> f (const x) undefined
* xはb型の値であり
	- const x :: a -> b
	- f (const x) :: a -> c
	- f (const x) undefined :: c

一引数関数(まとめ)
------------------

* 以下の2つの関数はほとんど同じものと考えることができる
	fun :: b -> c
	fun' :: (a -> b) -> (a -> c)
	fun' = (fun .)
* 「引数と返り値の両方に1つ引数を追加する」変換
* それぞれの変換を行う関数addArg, rmArgを定義した
* addArgは関数合成(.)と同じものだった
* 以下のような形の型を見たらより単純な関数に置き換え可
	(a -> Int) -> (a -> Char)
	(a -> [Int]) -> (a -> [Bool])
	(a -> Int -> Char) -> (a -> Bool)

二引数関数
----------

* 以下の関数を考える
	myAdd :: (a -> Int) -> Int -> (a -> Int)
	myAdd f y = \\x -> f x + y
* 関数と整数を取り
	「引数に関数を適用した結果に整数を足す」関数を返す
* transFuns.hsに書き込み、:reloadする
	*Main> (myAdd (* 2) 3) 8
	itext t 1 $ show $ (myAdd (* 2) 3) 8

二引数関数
----------

* より一般的には以下のようになる
	fun :: b -> c -> d
	fun' :: (a -> b) -> c -> (a -> d)
	fun' f y = \\x -> fun (f x) y
* 「第一引数と返り値に引数を1つ追加する」変換
	itext t 0 "addArg2 :: (b -> c -> d) -> (a -> b) -> c -> (a -> d)
	itext t 0 "addArg2 fun f y = \\x -> fun (f x) y
* これをポイントフリースタイルにすると
	addArg2 = (. flip (.)) . flip (.) . flip
* この変換には追加の情報は必要ないことがわかる

二引数関数
----------

* 逆方向の変換
	itext t 0 "rmArg2 :: ((a -> b) -> c -> (a -> d)) -> (b -> c -> d)
	itext t 0 "rmArg2 f = \\x y -> f (const x) y undefined
* x, yの型はそれぞれb, c
	- const x :: a -> b
	- f (const x) :: c -> (a -> d)
	- f (const x) y :: a -> d
	- f (const x) y undefined :: d

二引数関数(まとめ)
------------------

* 以下の2つの関数はほぼ同じ
	fun :: b -> c -> d
	fun' :: (a -> b) -> c -> (a -> d)
	fun' f y = \\x -> fun (f x) y
* 第一引数と返り値に引数を1つ追加する関数
	addArg2 fun f y = \\x -> fun (f x) y
* 以下の型を見たらより簡単な型に変換可
	(a -> Char) -> Int -> (a -> Bool) 
	(a -> [Bool]) -> [Char] -> (a -> [Int])
	(a -> Char) -> (Double -> Bool) -> (a -> Int)

まとめ
------

* 意味がほとんど変化しない変換を見てきた
* これらは片方が存在すればもう一方を導ける
* 複雑なほうの型の関数を作る場合
	- より簡単なほうの型の関数から変換するほうが
	- コードがすっきリするだろう
* 以下の変換を次回の「モナド」の回で使う
	b -> c -> d
	arrowIText t 1 "(a -> b) -> c -> (a -> d)
* これは後者の形の関数が必要になった場合
	前者の形の関数から導出できるということを意味する
* 引数と結果の関数に引数として同じ型変数がある場合
	- それを削除できる、と考えておこう

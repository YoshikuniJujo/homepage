第16回 モナド
=============

あいうえお

はじめに
--------

* (a -> m b)の形をとるいろいろな関数が存在する
* この形の関数の多くは(a -> m b)と(b -> m c)をつないで
	(a -> m c)を導出できると便利なことが多い
* 例としてMaybeについて考えてみる
* 小文字のみ文字コードにする関数と偶数のみを2で割る関数
	lowerToCode :: Char -> Maybe Int
	evenDiv2 :: Int -> Maybe Int
* これらをつないで
	- 小文字の文字コードで偶数のものの半分の値
	lowerToCodeDiv2 :: Char -> Maybe Int
* のような感じ

新たに必要になる構文
--------------------

* let [定義] in [表現]という形の構文がある
* [定義]中で定義された変数は[表現]のなかで使える
* 全体の値は[表現]によって表される
* 試してみる
	% ghci
	Prelude> let x = 8 in x * x
	64
	Prelude> x
	
	<interactive>:3:1: Not in scope: `x'

新たに必要になる構文
--------------------

* newtypeという構文がある
* 使いかたとしてはdata構文とほとんど同じである
	newtype [型構築子] [型変数1] [型変数2] ... = 
		[値構築子] [型]
* dataとの違い
	- 値構築子がひとつしか存在できず
	- 値構築子が型をひとつしか取れない
* つまりnewtypeで作られる型は、他の型のラッパーとなる
* 内部的にはもとの型と同じ型が使われる
	arrowIText t 1 "効率の低下がない

新たに必要になる構文
--------------------

* dataやnewtypeのフィールドに名前をつける構文がある
* 以下のような定義を見てみよう
	data Human = Human String Int
	name (Human n _) = n
	age (Human _ a) = a
* このように書く代わりに以下のように書くことができる
	data Human = Human { name :: String, age :: Int }
* フィールドを取り出す関数を用意してくれるということ

Maybe
-----

* さっきの例を実際に作ってみる
* lectures/lecture16/maybe.hsを作成し編集しよう
* Data.CharのisLower, ordを使うので
	import Data.Char (isLower, ord)
* 小文字のみ文字コードにする関数
	lowerToCode :: Char -> Maybe Int
	lowerToCode c
		| isLower c = Just $ ord c
		| otherwise = Nothing
* maybe.hsに書き込もう


Maybe
-----

* 偶数のみを2で割る関数
	evenDiv2 :: Int -> Maybe Int
	evenDiv2 n
		| even n = Just $ n `div` 2
		| otherwise = Nothing
* 以上をmaybe.hsに書き込もう
* この2つの関数をつなげよう
	lowerToCode :: Char -> Maybe Int
	evenDiv2 :: Int -> Maybe Int
* lowerToCodeが返すIntをevenDiv2の引数にしたい
	lowerToCodeDiv2 :: Char -> Maybe Int

Maybe
-----

* lowerToCodeDiv2を定義しよう
	lowerToCodeDiv2 :: Char -> Maybe Int
	lowerToCodeDiv2 c = case lowerToCode c of
		Just n -> evenDiv2 n
		Nothing -> Nothing
* maybe.hsに書き込もう
	% ghci maybe.hs
	*Main> lowerToCodeDiv2 'n'
		itext t 1 $ show $ lowerToCodeDiv2 'n', \t -> do
	*Main> lowerToCodeDiv2 'm'
		itext t 1 $ show $ lowerToCodeDiv2 'm'

Maybe
-----

* 4で割り切れるもののみを4で割るようにしてみる
* 以下の3つをつなぐ
	lowerToCode :: Char -> Maybe Int
	evenDiv2 :: Int -> Maybe Int
	evenDiv2 :: Int -> Maybe Int
* 定義は以下のようになる
	lowerToCodeDiv4 :: Char -> Maybe Int
	lowerToCodeDiv4 c = case lowerToCode c of
		Just n -> case evenDiv2 n of
			Just n' -> evenDiv2 n'
			Nothing -> Nothing
		Nothing -> Nothing

Maybe
-----

* どのように「つないだ」かを考えよう
	f :: a -> Maybe b
	g :: b -> Maybe c
* 関数fの結果がJust xならばxの値にgを適用する
	- その結果はJust yまたはNothingとなる
* 関数fの結果がNothingなら結果もNothing

Maybe
-----

* Maybe型を返す関数をつなぐpipeMを書く
	pipeM ::
	(a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
	f `pipeM` g = \\v -> case f v of
	Just x -> g x
	Nothing -> Nothing
* maybe.hsに書き込もう

Maybe
-----

* pipeMを使うとlowerToCodeDiv4は以下のように書ける
	lowerToCodeDiv4 :: Char -> Maybe Int
	lowerToCodeDiv4 =
	lowerToCode `pipeM` evenDiv2 `pipeM` evenDiv2
* maybe.hsに書き込み、:reload
	*Main> lowerToDiv4 'n'
		itext t 1 $ show $ lowerToCodeDiv4 'n', \t -> do
	*Main> lowerToDiv4 'p'
		itext t 1 $ show $ lowerToCodeDiv4 'p'


Maybe
-----

* 2で割ったうえに3をかけることを考える
* かけ算はとくに割り切れないとかがないので
	mul3 :: Int -> Int
	mul3 = (* 3)
* しかし、pipeMでつなぐには以下の形のほうが良い
	Int -> Maybe Int
* この変換を行う関数を作ろう
	arrM :: (a -> b) -> (a -> Maybe b)
	arrM f = \\x -> Just $ f x
* mul3とarrMをmaybe.hsに書き込もう


Maybe
-----

* 小文字のコードを2で割ったうえに3をかける関数は
	lowerToCodeDiv2Mul3 :: Char -> Maybe Int
	lowerToCodeDiv2Mul3 =
	lowerToCode `pipeM` evenDiv2 `pipeM` arrM mul3
* maybe.hsに書き込み、:reloadする
	*Main> lowerToCodeDiv2Mul3 'n'
		itext t 1 $ show $ lowerToCodeDiv2Mul3 'n'

Maybe
-----

* (a -> Maybe b)型の関数をつなぐために用意した関数
	pipeM :: (a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
	arrM :: (a -> b) -> (a -> Maybe b)
* 前回の講義を思い出してみよう
* 引数の型と結果の型の両方に'a ->'があるので
	- それらを消すことができる
	bindM :: Maybe b -> (b -> Maybe c) -> Maybe c
	retM :: b -> Maybe b
* こちらのセットを定義してみよう

Maybe
-----

* それぞれの関数を定義する
	bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
	bindM (Just x) f = f x
	bindM Nothing _ = Nothing
	retM :: a -> Maybe a
	retM = Just
* maybe.hsに書き込もう


Maybe
-----

* このセットを使って、lowerToCodeDiv2Mul3'を定義してみる
	lowerToCodeDiv2Mul3' :: Char -> Maybe Int
	lowerToCodeDiv2Mul3' c =
		lowerToCode c `bindM` evenDiv2 `bindM` (retM . mul3)
* この関数を以下のように書くこともできる
	lowerToCodeDiv2Mul3' c =
	lowerToCode c `bindM` \\n ->
	evenDiv2 n `bindM` \\n' ->
	retM $ mul3 n'
* この形だと、lowerToCode cの結果にnを束縛し
	- evenDiv2 nの結果にn'を束縛し
	- mul3 n'の値を返す、と読める
* どちらかをmaybe.hsに書き込もう


Maybe
-----

* 試してみる
	*Main> :reload
	*Main> lowerToCodeDiv2Mul3' 'p'
	itext t 1 $ show $ lowerToCodeDiv2Mul3' 'p'

Maybe
-----

* 2つめの形を再掲する
	lowerToCodeDiv2Mul3' c =
		lowerToCode c `bindM` \\n ->
		evenDiv2 n `bindM` \\n' ->
		retM $ mul3 n'
* 適切に括弧をつけると以下のようになる
	lowerToCode c `bindM` (\\n ->
	evenDiv2 n `bindM` (\\n' ->
		retM $ mul3 n'))

Maybe(まとめ)
-------------

* 失敗するかもしれない計算
	a -> Maybe b
* そういった計算を「つなぐ」関数をつくると
	(a -> Maybe b) -> (b -> Maybe c) -> (a -> Maybe c)
* きれいに抽象化できる
* 普通の計算も同じ形に直すことで「つなぐ」
	(a -> b) -> (a -> Maybe b)
* これらの関数はより単純な形に直すことができる
	bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
	retM :: a -> Maybe a

State
-----

* メモリ機能付きの電卓について考えてみよう
* (3 * 4 + 2 * 5) * 7の計算をしてみる
* 以下の順にボタンを押す
	3 * 4 M+ C 2 * 5 M+ C MR * 7
	- 3 * 4を計算しメモリに足す
	- 表示をクリアする
	- 2 * 5を計算しメモリに足す
	- 表示をクリアする
	- メモリを呼び出す
	- 7をかける
* メモリ内の記憶を状態として持っていると考えられる

State
-----

* それぞれの操作は
	- 画面の表示とメモリの状態を引数として取り
	- 画面の表示とメモリの状態を返値として返す
* 実際の電卓とは異なるが
	- M+は画面をクリアするものとする(つまりCも行う)
	- MRを押す前に画面がクリアされている必要がある
	- つまり、MRは画面の状態を受け取らない
* よって、それぞれの型は以下のようになる
	mplus :: Int -> Int -> ((), Int)
	mrecall :: () -> Int -> (Int, Int)
* それぞれ返値と引数の画面の表示の部分が()としてある
* 表示がないということを()で表現している

State
-----

* 次に画面を変化させる関数を作る
* (Int -> Int)関数を変換することによって作ることにする
* メモリは変化させない、つまり
	- メモリの値を引数として取り、そのまま返り値とする
	arrC :: (Int -> Int) -> Int -> Int -> (Int, Int)
* arrCは単なる数字キーにも対応させたい
* 数字キーは引数を取らないでIntを返す関数なので
	() -> Int
	arrC :: (() -> Int) -> () -> Int -> (Int, Int)
* よってarrCはより一般的に以下のようにする
	arrC :: (a -> Int) -> a -> Int -> (Int, Int)

State
-----

* arrCを画面のクリアにも使えるようにしたい
* 画面をクリアする関数は(Int -> ())
	arrC :: (Int -> ()) -> Int -> Int -> ((), Int)
* さっきまでのarrCの型が以下のようになっていたので
	arrC :: (a -> Int) -> a -> Int -> (Int, Int)
* 十分に一般的なarrCの型は以下のようになる
	arrC :: (a -> b) -> a -> Int -> (b, Int)

State
-----

* 必要な関数は以下のようになる
	mplus :: Int -> Int -> ((), Int)
	mrecall :: () -> Int -> (Int, Int)
	arrC :: (a -> b) -> a -> Int -> (b, Int)
* mplusを定義してみよう
	- 画面の表示とメモリの内容を足してメモリに保存する
	- 画面の表示はクリアする
	mplus x m = ((), x + m)

State
-----

* calc.hsを作成し以下を書き込もう
	mplus :: Int -> Int -> ((), Int)
	mplus x m = ((), x + m)
* mrecallを定義する
	- メモリの内容を画面に呼び出す
	- 呼び出した直後はメモリと画面は同じ値になる
	mrecall :: () -> Int -> (Int, Int)
	mrecall _ m = (m, m)
* これもcalc.hsに書き込もう

State
-----

* 試してみる
	*Main> :load calc.hs
	*Main> mplus 3 0
	itext t 1 $ show $ mplus 3 0, \t -> do
	*Main> mplus 4 8
	itext t 1 $ show $ mplus 4 8, \t -> do
* 画面の表示が3でメモリが0のときmplusをすると
	- 画面はクリア(ユニット値, 0ではなく())され
	- メモリは3となる
* 画面の表示が4でメモリが8のときmplusをすると
	- 画面はクリアされ、メモリは12となる

State
-----

* 試してみる
	*Main> mrecall () 4
		itext t 1 $ show $ mrecall () 4, \t -> do
	*Main> mrecall () 19
		itext t 1 $ show $ mrecall () 19, \t -> do
* mrecallをする前には画面がクリアされている必要がある
	- つまり画面はユニット値になっていなければならない
* mrecallをするとメモリの内容が画面に呼び出される
	- mrecallの直後はメモリの内容と画面は同じ値になる

State
-----

* 次に必要なのは関数によって電卓の表示を変化させる関数
	arrC :: (a -> b) -> a -> Int -> (b, Int)
* メモリの値は変化させない、つまり
	- 引数として取りそのまま返す
* 画面の値は与えられた関数で変化させる
	arrC f x m = (f x, m)
* これらをcalc.hsに書き込もう

State
-----

* 試してみよう
* まずは単なる数字を入れてみる
* 単なる数字を入れるには
	- 画面の値を無視して数を返す関数を使えば良いので
	*Main> :reload
	*Main> arrC (const 8) () 4
		itext t 1 $ show $ arrC (const (8 :: Int)) () 4, \t -> do
	*Main> arrC (const 11) () 32
		itext t 1 $ show $ arrC (const (11 :: Int)) () 32, \t -> do
* メモリの値は変化せずに表示が与えられた数となる
* 数字を入れる前に表示はクリアされている

State
-----

* 次は画面に数字が表示されている状態で
	- それに対する演算を行ってみる
	*Main> arrC (* 3) 2 5
		itext t 1 $ show $ arrC (* (3 :: Int)) 2 5, \t -> do
	*Main> arrC (+ 8) 7 23
		itext t 1 $ show $ arrC (+ (8 :: Int)) 7 23, \t -> do
* メモリの値は変化しない
* 画面の値に与えられた演算が行われている

State
-----

* arrCは十分に一般的に作ったので四則演算以外もできる
	*Main> arrC even 4 7
		itext t 1 $ show $ arrC even (4 :: Int) 7, \t -> do
	*Main> :m + Data.Char
	*Main Data.Char> arrC chr 99 37
		itext t 1 $ show $ arrC chr 99 37, \t -> do
* 電卓のクリアキーは画面の値を無視して
	- 画面の値をクリア(ユニット値, ())するので
	*Main Data.Char> arrC (const ()) 37 8
		itext t 1 $ show $ arrC (const ()) (37 :: Int) 8

State
-----

* 今まで扱ってきた関数は共通の形を持っている
* その共通部分を取り出すと以下のようになる
	a -> Int -> (b, Int)
	- aは直前の画面の値、1つめのIntは直前のメモリの値
	- bは直後の画面の値、2つめのIntは直後のメモリの値
* 今後この型を頻繁に用いるので別名をつけておこう
	type Calc a b = a -> Int -> (b, Int)
* たとえば以下のようになる
	mplus :: Calc Int ()
	mrecall :: Calc () Int
	arrC even :: Calc Int Bool
	arrC chr :: Calc Int Char

State
-----

* 計算の部品はそろったので次はそれを組み合わせよう
* 組み合わせるための関数の型は以下の形となるはずだ
	pipeC :: Calc a b -> Calc b c -> Calc a c
	- 画面の値をaからbにする計算と
	- 画面の値をbからcにする計算とをつないで
	- 画面の値をaからcにする計算をつくる
* 中身は以下のようになる
	f `pipeC` g = \\x m -> let (x', m') = f x m in g x' m'
* let X in Yの形でXのなかで束縛した変数をYのなかで使える
* はじめの画面の値xとメモリの値mをfに与え
	- その結果をx', m'に束縛し
	- x', m'をgに与えている

State
-----

* これらをcalc.hsに書き込もう
	type Calc a b = a -> Int -> (b, Int)
	pipeC :: Calc a b -> Calc b c -> Calc a c
	f `pipeC` g = \\x m -> let (x', m') = f x m in g x' m'
* 試してみよう
	*Main> :reload
	*Main> (arrC (const 3) `pipeC` arrC (* 2)) () 23
		itext t 1 $ show $ (arrC (const (3 :: Int)) `pipeC` arrC (* 2)) () 23, \t -> do
	*Main> (arrC (const 4) `pipeC` mplus) () 3
		itext t 1 $ show $ (arrC (const 4) `pipeC` mplus) () 3

State
-----

* 最初の例の(3 * 4 + 2 * 5) * 7を作ってみる
	example :: Calc () Int
	example =
		arrC (const 3) `pipeC`
		arrC (* 4) `pipeC`
		mplus `pipeC`
		arrC (const 2) `pipeC`
		arrC (* 5) `pipeC`
		mplus `pipeC`
		mrecall `pipeC`
		arrC (* 7)
* これをcalc.hsに書き込もう

State
-----

* 試してみよう
	*Main> :reload
	*Main> example () 0
	itext t 1 $ show $ example () 0, \t -> do
* 初期状態は
	- 画面はクリア(ユニット値, ())されていてメモリは0
* それを引数として与えている

State
-----

* Calc型を見てみる
	type Calc a b = a -> Int -> (b, Int)
* State型を作ると
	type State b = Int -> (b, Int)
	type Calc a b = a -> State b
* a -> bとa -> State bを比較してみる
	- a -> bは画面の値の変化
	- a -> State bは画面とメモリの変化
	- 画面の値の変化にメモリの値の変化が追加されている

State
-----

* pipeCの型をState型を使って書き換えてみる
	type State a = Int -> (a, Int)
	pipeC ::
	(a -> State b) -> (b -> State c) -> (a -> State c)
* 画面とメモリを変化させる関数をつないでいる
* 画面の変化については明示的に示されているが
	- メモリの変化についてはState型に隠されている
* pipeCを見ると1つめと2つめのa型は同じ値なので消せる
	bindC :: State b -> (b -> State c) -> State c

State
-----

* bindCを定義する
	bindC :: State a -> (a -> State b) -> State b
	bindC f g = \\m -> let (x, m') = f m in g x m'
	- fに状態mを与え結果の値と状態をgに与えている
* 同様にarrCも以下のようにできる
	arrC :: (a -> b) -> (a -> State b)
		=> retC :: b -> State b
	retC x = \\m -> (x, m)
	- 状態は変化させずに値xを返す

State
-----

* これらをcalc.hsに書き込もう
	type State a = Int -> (a, Int)
	bindC :: State a -> (a -> State b) -> State b
	bindC f g = \\m -> let (x, m') = f m in f x m'
	retC :: a -> State a
	retC x = \\m -> (x, m)


State
-----

* 最初の例(3 * 4 + 2 * 5) * 7をbindC, retCで書いてみる
	example' :: State Int
	example' =
		retC 3 `bindC`
		(retC . (* 4)) `bindC`
		mplus `bindC`
		const (retC 2) `bindC`
		(retC . (* 5)) `bindC`
		mplus `bindC`
		mrecall `bindC`
		(retC . (* 7))
* これをcalc.hsに書き込もう

State
-----

* 試してみる
	*Main> :reload
	*Main> example' 0
	itext t 1 $ show $ example' 0

State
-----

* 同じことを以下のように書くこともできる
* calc.hsに書き込もう
	example'' =
		retC 3 `bindC` \\x ->
		retC (x * 4) `bindC` \\y ->
		mplus y `bindC` \\_ ->
		retC 2 `bindC` \\z ->
		retC (z * 5) `bindC` \\w ->
		mplus w `bindC` \\_ ->
		mrecall () `bindC` \\v ->
		retC (v * 7)


State
-----

* これは以下のように読むことができる
	- retC 3で返る値でxを束縛し
	- retC (x * 4)で返る値でyを束縛し
	- mplus yでyの値を状態に足し返り値は捨て
	- retC 2で返る値でzを束縛し
	- retC (z * 5)で返る値でwを束縛し
	- mplus wでwの値を状態に足し返り値は捨て
	- mrecall ()で状態の値を呼び出し、vを束縛し
	- retC (v * 7)の値を返す

State(まとめ)
-------------

* メモリ付き電卓の例を見た
* 画面の値とメモリの値のペアを次々と変換していく
* 画面の値にだけ注目し、メモリの値を隠すことができた
* 以下の関数で変換を部品としてつないでいくことができる
	pipeC ::
	(a -> State b) -> (b -> State c) -> (a -> State c)
	arrC :: (a -> b) -> (a -> State b)
* これは以下のように簡略化できる
	bindC :: State a -> (a -> State b) -> State c
	retC :: a -> State a

MaybeとState
------------

* Maybeをつなぐときに使った関数
	retM :: a -> Maybe a
	bindM :: Maybe a -> (a -> Maybe b) -> Maybe b
* Stateをつなぐときに使った関数
	retC :: a -> State a
	bindC :: State a -> (a -> State b) -> State b
* これらの型はMaybeとStateを置き換えただけになっている
* 共通する構造を抽出すると以下のようになる
	ret :: a -> m a
	bind :: m a -> (a -> m b) -> m b

モナド
------

* この2つの関数を持つ型mをモナドと呼ぶ
	ret :: a -> m a
	bind :: m a -> (a -> m b) -> m b
* これらは以下の関数の簡略化したものと考えて良い
	arr :: (a -> b) -> (a -> m b)
	pipe :: (a -> m b) -> (b -> m c) -> (a -> m c)
* モナドとするには以下の法則を満たす必要がある
	1. ret `pipe` fはfと同じ
	2. f `pipe` retはfと同じ
	3. (f `pipe` g) `pipe` hとf `pipe` (g `pipe` h)は同じ
* これをモナド則と呼ぶ

モナド
------

* モナド則は簡単に言うと以下のことを言っている
	- retは値を包み込むだけでそれ以外のことをしない
	- 変換関数を左結合にしても右結合にしても同じ
* retを空文字列とし関数を文字列として見るとわかりやすい
	\"\" ++ \"hello\" == \"hello\
	\"hello\" ++ \"\" == \"hello\
	(\"hello\" ++ \"my\") ++ \"friend\" ==
		\"hello\" ++ (\"my\" ++ \"frind\")

モナド
------

* 何であれ以下の型の関数が存在し
	a -> m a
	m a -> (a -> m b) -> m b
* それがモナド則を満たしさえすれば、型mはモナドである
* MaybeやStateはモナドである
* 中身が何であれ関係なくすべてモナドである
* 「モナド」とは内容ではなく形式である
* MaybeとStateのあいだにはほとんど共通点はない
	- ただモナドという形式を満たすというだけ

モナド
------

* 同じことだが、よりイメージしやすいモナド関数の型
	(a -> b) -> (a -> m b)
	(a -> m b) -> (b -> m c) -> (a -> m c)
* 言葉で言うとこうなる
	- 普通の関数を「値をモナドにする関数」に変換できる
	- 「値をモナドにする関数」同士をつなぐことができる

Monadクラス
-----------

* 値を比較できるもののためにEqクラスが用意されている
* 同様にモナドに対してはMonadクラスが用意されている
* クラス定義は以下のようになっている
	class Monad m where
		(>>=) :: m a -> (a -> m b) -> m b
		return :: a -> m a
* つまり(>>=)とreturnを定義してやれば
	- Monadのインスタンスにすることができる

Maybeモナド
-----------

* Maybe型はデフォルトでモナドクラスのインスタンスである
* instance宣言は以下のようになっている
	instance Monad Maybe where
		Nothing >>= _ = Nothing
		Just x >>= f = f x
		return = Just

Maybeモナド
-----------

* maybe.hsに以下を書き込んでみよう
	lowerToCodeDiv4' :: Char -> Maybe Int
	lowerToCodeDiv4' =
		lowerToCode c >>= evenDiv2 >>= evenDiv2
* 試してみる
	*Main> :load maybe.hs
	*Main> lowerToCodeDiv4' 'n'
		itext t 1 $ show $ lowerToCodeDiv4' 'n'
	*Main> lowerToCodeDiv4' 'p'
		itext t 1 $ show $ lowerToCodeDiv4' 'p'

Maybeモナド
-----------

* 同じことだが以下のように書くこともできる
	lowerToCodeDiv4'' c =
		lowerToCode c >>= \\n ->
		evenDiv2 n >>= \\n' ->
		evenDiv2 n'
* これは以下のように読める
	- lowerToCode cが返す値でnを束縛して
	- evenDiv2 nが返す値でn'を束縛して
	- evenDiv2 n'の値を返す

Maybeモナド
-----------

* do記法という構文糖がある
* それを使うと同じことが以下のように書ける
	lowerToCodeDiv4''' c = do
		n <- lowerToCode c
		n' <- evenDiv2 n
		evenDiv2 n'
* doという識別子で始める
* それぞれの行で以下の変換が行われる
	[変数] <- [表現]
	arrowIText t 1 "[表現] >>= \\[変数] ->


Stateモナド
-----------

* calc.hsで定義したStateは単なる別名
	type State a = Int -> (a, Int)
* 別名に対してはinstance宣言はできない
* 別名をつける代わりにnewtypeを使うことができる
* newtypeは内部的にはtypeと同じだが
	- 使いかたとしてはdataと同様に使える
* dataを使っても使いかたはほとんど同じだが
	- newtypeを使ったほうが実行効率は良くなる
* newtypeを使いMonadクラスのインスタンスとする
* state.hsに書き込んでいこう


Stateモナド(コメントアウト部分)
-------------------------------

* ここでひとつ省略記法を紹介する
* dataやnewtype宣言のなかでフィールド名を指定できる
	data Human = Human String Int
	name (Human n _) = n
	age (Human _ a) = a
* 上のように書く代わりに以下のように書ける
	data Human = Human { name :: String, age :: Int }
* フィールドを取り出す関数を用意してくれる

Stateモナド
-----------

* State型を定義しよう。state.hsに以下を書き込む
	newtype State a = State { runState :: Int -> (a, Int) }
* この定義は以下の2つの定義とだいたい同じ
	newtype State a = State (Int -> (a, Int))
	runState (State st) = st
* 簡単に言えばStateで服を着せて
	- runStateで服を脱がせるということ

Stateモナド
-----------

* Monadクラスのインスタンスにする
	instance Monad State where
	State m >>= f = State $ \\s ->
		let (x, s') = m s in runState (f x) s'
	return x = State $ \\s -> (x, s)
* 複雑に見えるがStateやrunStateを消して考えれば良い
	- それらは服を着せたり脱がせたりしているだけ
* state.hsに書き込もう

Stateモナド
-----------

* mplusはより一般的な形に直せる
* put, get関数を定義してみよう
	put :: Int -> State ()
	put s = State $ \\_ -> ((), s)
	get :: State Int
	get = State $ \\s -> (s, s)
* putは引数の値で「状態」を置き換える
* getは「状態」を「画面」にコピーする
	- mrecallと同じ
	text t "putとgetの定義をstate.hsに書き込もう

Stateモナド
-----------

* putとgetを使ってmodifyが定義できる
	modify :: (Int -> Int) -> State ()
	modify f = get >>= put . f
* mplusはmodifyを使って定義できる
	mplus :: Int -> State ()
	mplus x = modify (+ x)
* state.hsに書き込もう

Stateモナド
-----------

* 最初の例(3 * 4 + 2 * 5) * 7を作ってみよう
* state.hsに書き込もう
	example :: State Int
	example =
		return 3 >>=
		return . (* 4) >>=
		mplus >>=
		const (return 2) >>=
		return . (* 5) >>=
		mplus >>=
		const get >>=
		return . (* 7)

Stateモナド
-----------

* 試してみよう
	*Main> :load state.hs
	*Main> runState example 0
		itext t 1 $ show $ runState example''' 0, \t -> do
* runStateで服を脱がせたうえで
	- 初期値の0を与えている

Stateモナド
-----------

* exampleの定義のなかで2ヶ所にconstが出てきた
* これは直前の計算の返り値を使わないということ
* 次の計算に返り値を渡さない場合
	- (>>=)の代わりに(>>)を使うと良い
	(>>=) :: m a -> (a -> m b) -> m b
	(>>) :: m a -> m b -> m b
* (>>)の定義は以下のようになる
	m1 >> m2 = m1 >>= const m2
* 以下のように書いても同じこと
	m1 >> m2 = m1 >>= \\_ -> m2

Stateモナド
-----------

* (>>)を使ってexampleを書き換えると以下のようになる
	example' =
		return 3 >>=
		return . (* 4) >>=
		mplus >>
		return 2 >>=
		return . (* 5) >>=
		mplus >>
		get >>=
		return . (* 7)

Stateモナド
-----------

* 明示的な局所変数を使った書き換え
	example'' =
		return 3 >>= \\x ->
		return (x * 4) >>= \\y ->
		mplus y >>
		return 2 >>= \\z ->
		return (z * 5) >>= \\w ->
		mplus w >>
		get >>= \\v ->
		return (v * 7)

Stateモナド
-----------

* do記法を使った書き換え
	example''' = do
		x <- return 3
		y <- return $ x * 4
		mplus y
		z <- return 2
		w <- return $ z * 5
		mplus w
		v <- get
		return $ v * 7

Stateモナド
----------

* [変数] <- return [値]という形が出てきたが
	- この形にはlet [変数] = [値]という構文糖が使える
	- また、連続するletはひとつにまとめられる
	example''' = do
		let
	preLine t
			x = 3
			y = x * 4
		mplus y
		let
	preLine t
			z = 2
			w = z * 5
		mplus w
		v <- get
		return $ v * 7

まとめ
------

* (a -> m b)の形の関数を結合する規則が存在し
	- それがモナド則を満たせばmはモナドである
* 以下の型の関数が必要である
	m a -> (a -> m b) -> m b
	a -> m a
* 条件を満たせば何でもモナド
* 今回はMaybeモナドとStateモナドを見た
* その2つは中身は大きく異なるがともにモナドである
* Monadクラスが用意されている
* Monadクラスのインスタンスにするとdo記法が使える

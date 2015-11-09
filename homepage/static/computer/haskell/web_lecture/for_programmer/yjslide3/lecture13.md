第13回 型クラス
===============

はじめに
--------

* 複数の型に共通の性質が存在する
	- 大小比較ができること
	- 文字列として表示できること
	- 等々
* ある型がその「性質」を持つということは
	- それを表現する関数が型に対して定義されていること
* つまり(>)や(<)がIntに対して定義されているので
	- Int型は「大小比較可」という性質を持つ

型クラス
--------

* Haskellでは、そのような性質を型クラスで表現する
* 型TがクラスCの性質を持つということを
	- 「型TはクラスCのインスタンスである」と表現する
* 型クラスの例
	- Ordは「大小比較可」という性質を表すクラスであり
	- Showは「表示可」という性質を表すクラスである
* インスタンスの例
	- Int型はOrdクラスのインスタンスである
	- Char型はShowクラスのインスタンスである
	- 等々

型クラスのチェック
------------------

* コマンドプロンプトを2つ立ち上げて
* lectures/lecture13を作りそこに移動しよう
	% ghci
	Prelude> :info Char
	...
	instance Ord Char ...
	...
	instance Show Char ...
* その型がどのクラスのインスタンスであるかを調べるには
	- ghciで:info [型名]とする
* CharがOrdとShowのインスタンスであることがわかった

クラス関数のチェック
--------------------

* ある型クラスにどんなクラス関数があるかを知るには
	Prelude> :info Ord
	class Eq a => Ord a where"
		compare :: a -> a -> Ordering"
		(<) :: a -> a -> Bool"
		...
* Ordクラスにはcompare, (<)などがある
* また'Eq a =>'とあるのは以下を意味する
* Ordクラスのインスタンスであるためには
	Eqクラスのインスタンスである必要がある

自作の型をOrdとする
------------------

* 型クラスは自分で作れるが
* まずは自作の型を既存の型クラスのインスタンスにしよう
* スターバックスのカップのサイズを表す型を作る
	data Size = Short | Tall | Grande | Venti
* これをclass.hsに保存しよう
* これは大小比較可なのでOrdクラスのインスタンスにしよう
* さっき見たようにOrdクラスのインスタンスにするためには
	- Eqクラスのインスタンスにする必要がある
* Eqクラスは同値かどうか判定可という性質を表現するクラス"

自作の型をEqとする
------------------

* Eqクラスのクラス関数には(==), (/=)がある
* このクラスのインスタンスにするには(==)を定義する
* (==)を定義しておけば(/=)にはデフォルトの定義が使われる
* 実行効率あるいはその他の理由で(/=)を別に定義することも
* SizeをEqクラスのインスタンスにしてみよう
	instance Eq Size where
		Short == Short = True
		Tall == Tall = True
		Grande == Grande = True
		Venti == Venti = True
		_ == _ = False
* これをclass.hsに書き込もう"

試してみる
----------

* 試してみよう
	Prelude> :load class.hs
	*Main> Short == Short
	itext t 1 $ show $ Short == Short
	*Main> Tall == Venti
	itext t 1 $ show $ Tall == Venti
	*Main> Grande /= Grande
	itext t 1 $ show $ Grande /= Grande
	*Main> Venti /= Short
	itext t 1 $ show $ Venti /= Short

自作の型をOrdにする
-------------------

* Ordクラスには7つのクラス関数がある
	compare, (<), (>=), (>), (<=), max, min
* すべて定義しても良いが
	- compareまたは(<=)のどちらかを定義すれば
	- 他の関数はデフォルトの値が使われる"

自作の型をOrdにする
-------------------

* SizeをOrdのインスタンスにする
	instance Ord Size where
		Short <= _ = True
		_ <= Short = False
		Tall <= _ = True
		_ <= Tall = False
		Grande <= _ = True
		_ <= Grande = False
		Venti <= _ = True
* これをclass.hsに書き込もう"

試してみる

* 試してみよう
	*Main> :reload
	*Main> Short < Short
	itext t 1 $ show $ Short < Short
	*Main> Grande <= Grande
	itext t 1 $ show $ Grande <= Grande
	*Main> Tall >= Venti
	itext t 1 $ show $ Tall >= Venti
	*Main> Venti > Short
	itext t 1 $ show $ Venti > Short

ここまでのまとめ
----------------

* 複数の型に共通した性質がある
* 性質はその型を扱う関数によって表される
* そのような性質、つまり関数をまとめたものが型クラス
* 型クラスのインスタンスにするには関数の中身を定義する
* 構文は以下のようになる
	instance [型クラス] [型] where
		[関数定義1]
		[関数定義2]
		..."

deriving
--------

* 実はさっきやったことは以下の定義で実現できる
	data Size = Short | Tall | Grande | Venti
		deriving (Eq, Ord)
* 使用頻度の高い以下のクラスについてはderivingで簡単にインスタンスを導出できる
	Eq, Ord, Enum, Ix, Bounded, Show, Read
* derivingを使う場合は値構築子を「小さいものから大きいものへ」の順に並べる

オートマトン
------------

* オートマトンというものがある
* 入力によって次々に状態を変化させていくもの
* 初期状態と受理状態とがある
* 初期状態に対して入力を次々と与えていき
	- 入力が終わったときに状態が受理状態であれば
	- その入力列を受理するという
* 単純な機械をモデル化したものと考えられる
* これを型クラスという仕組みを活用して作ってみよう"

オートマトン
------------

* 状態を丸で表し
* 状態間の遷移を矢印で表す
	- 矢印のそばにその遷移を引き起こす入力を書く
* 初期状態には矢印を追加する
* 受理状態は二重丸とする
* 例:
	m1 t 100 240
* 入力列が0, 1, 1, 0, 0の場合
	q1 -> q1 -> q2 -> q2 -> q3 -> q2
* q2は受理状態なのでこの入力列は受理される

オートマトン
------------

* 今のオートマトンを再掲する
	flushoff t
	hideturtle t
	m1 t 100 140
	flush t
	showturtle t
	flushon t
* このオートマトンが受理するのは以下の入力列となる
	- すくなくともひとつの1を含み
	- 最後の1のあとには偶数個(0個も可)の0が来る
* いろいろな例で確認してみよう

オートマトン
------------

* 入力が1で終わる場合は受理
	1: q1 -(1)-> q2
	01: q1 -(0)-> q1 -(1)-> q2
	11: q1 -(1)-> q2 -(1)-> q2
	0101: q1 -(0)-> q1 -(1)-> q2 -(0)-> q3 -(1) -> q2
* どの状態でも1が来れば受理状態であるq2に遷移するので

オートマトン
------------

* 入力に1が含まれなければ受理されない
	(入力無し): q1
	0: q1 -(0)-> q1
	000: q1 -(0) -> q1 -(0)-> q1 -(0) -> q1
* q1から出るにはすくなくともひとつの1が必要

オートマトン
------------

* 最後の1の後に奇数個の0が続く場合受理されない
	10 : q1 -(1)-> q2 -(0)-> q3
	1000 : q1 -(1)-> q2 -(0) -> q3 -(0)-> q2 -(0)-> q3
* 最後の1の後に偶数個の0が続く場合は受理
	itext t 1 "100 : q1 -(1)-> q2 -(0) -> q3 -(0)-> q2
* 0が来るたびにq2とq3のあいだを行き来するので

オートマトン
------------

* 1が入力されなければq1にとどまる
* 1が来た時点で必ず状態q2にいる
* よって、最後の1以前の入力は無視できる
* 最後の1以降だけを考えれば良い
* 最後の1以降には0しか来ない
* 0が来るたびにq2とq3のあいだを行き来するので
	- 偶数個の0ならば受理され
	- 奇数個では受理されない"

オートマトン
------------

* ある型がオートマトンの状態であることを表す"
	AMStateクラスを作っていく
* 入力値は0, 1値とし、それを以下で表現する
	data OI = O | I deriving Show
	- アルファベットのOとIを0と1にみたてる
* これをautomaton.hsに書き込もう
* オートマトンの状態であるために必要なのは何かを考える
	1. ある状態が入力によってどの状態に移るか
	2. 初期状態
	3. 状態が受理状態であるかどうかのチェック

オートマトン
------------

* 状態を表す型を型変数qで表すことにする
* 「1. 状態の遷移」を表す関数stepが必要
	- これは現在の状態と入力値を引数とし
	- 遷移先の状態を返す関数なので
	=> step :: q -> OI -> q
* 「2. 初期状態」は「状態」なので
	=> start :: q
* 「3. 状態が受理状態であるかどうかのチェック」は
	- 状態をとりBool値を返せば良いので
	=> accept :: q -> Bool"

オートマトン
------------

* クラス宣言は以下のような構文となる
	class [クラス名] [型変数] where
		[クラス関数名1] :: [型1]
		[クラス関数名2] :: [型2]
		...

オートマトン
------------

* よってオートマトンの状態であることを示すクラスは
	class AMState q where
		step :: q -> OI -> q
		start :: q
		accept :: q -> Bool
* これはどんな型であっても
	- step, start, acceptを定義しさえすれば
	- オートマトンの状態となるということ

オートマトン
------------

* AMStateのインスタンスに対する関数を作ることができる
* 入力列に対する状態遷移の結果を返す関数
	run :: AMState q => q -> [OI] -> q
	run q [] = q
	run q (oi : ois) = run (step q oi) ois
* 入力がなければ状態はそのまま
* 入力があれば
	その入力に対して状態を遷移させたうえで入力を続ける
* automaton.hsに書き込もう

オートマトン
------------

* ある入力列が受理されるかどうかを判定する関数は
	isAccept :: AMState q => [OI] -> Bool
	isAccept = accept . run start
* で良さそうだが、これは動かない
* isAcceptの引数は[OI]で返り値はBoolである
	どこにもqが出てこない
	startの型が決められない
* ダミーの引数を使うことになる
	isAccept :: AMState q => q -> [OI] -> Bool
	isAccept q = accept . run (start `asTypeOf` q)
* automaton.hsに書き込もう

オートマトン
------------

* それでは最初の例を表すオートマトンを作ってみよう
	flushoff t
	hideturtle t
	m1 t 100 140
	flush t
	showturtle t
	flushon t
* 型をAM1としてその型の値をQ1, Q2, Q3とする
	data AM1 = Q1 | Q2 | Q3 deriving Show

オートマトン
------------

	flushoff t
	hideturtle t
	m1 t 100 120
	flush t
	showturtle t
	flushon t
* まずはstepAM1を定義しよう
	stepAM1 :: AM1 -> OI -> AM1
	stepAM1 Q1 O = Q1
	stepAM1 Q1 I = Q2
	stepAM1 Q2 O = Q3
	stepAM1 Q2 I = Q2
	stepAM1 Q3 _ = Q2
* automaton.hsに書き込もう

オートマトン
------------

* AMStateのインスタンスにする
	- stepはstepAM1
	- 初期状態はQ1
	- 受理状態は「Q2であること」なので
* インスタンス宣言は以下のようになる
	instance AMState AM1 where
		step = stepAM1
		start = Q1
		accept Q2 = True
		accept _ = False
* これをautomaton.hsに書き込もう

オートマトン
------------

* 試してみる前に
* isAcceptの使いかたを見てみよう
	isAccept :: AMState q => q -> [OI] -> Bool
* isAcceptの第一引数はダミーの引数で
	- 評価されることはない
	- 型だけわかればいい
* 評価されるとエラーを発生させるだけの値が用意されている
	undefined :: a
* undefinedはあらゆる型になれるので
* isAcceptの第一引数は(undefined :: AM1)とすれば良い

オートマトン
------------

* 今考えているオートマトンが受理する入力列は
	- すくなくともひとつの1を含み
	- 最後の1のあとには偶数個の0が並ぶ"

オートマトン
------------

	*Main> :load automaton.hs
	*Main> isAccept (undefined :: AM1) []
	isAccept (undefined :: AM1) [], \t -> do
	*Main> isAccept (undefined :: AM1) [O]
	isAccept (undefined :: AM1) [O], \t -> do
	*Main> isAccept (undefined :: AM1) [I]
	isAccept (undefined :: AM1) [I], \t -> do
	*Main> isAccept (undefined :: AM1) [I, I, O, I, O, O]
	isAccept (undefined :: AM1) [I, I, O, I, O, O], \t -> do
	*Main> isAccept (undefined :: AM1) [I, I, O, I, O, O, O]
	isAccept (undefined :: AM1) [I, I, O, I, O, O, O]

オートマトン
------------

* もうひとつ例を見てみよう
* 以下のようなオートマトンを考える
	- 1の個数が奇数個である入力列を受理する
* これは以下のようにして実現できる
	m2 t 100 220
* 1が来るたびに状態qeと状態qo間を遷移し
* 0の場合には同じ状態にとどまる
* 初期状態はqeであり、受理状態はqo

オートマトン
------------

* AM2を定義しよう
	data AM2 = Qe | Qo deriving Show
* stepAM2を定義する
	stepAM2 :: AM2 -> OI -> AM2
	stepAM2 q O = q
	stepAM2 Qe I = Qo
	stepAM2 Qo I = Qe
* automaton.hsに書き込もう

オートマトン
------------

* AM2をAMStateのインスタンスにする
	instance AMState AM2 where
		step = stepAM2
		start = Qe
		accept Qo = True
		accept _ = False
* automaton.hsに書き込もう

オートマトン
------------

	*Main> :reload
	*Main> isAccept (undefined :: AM2) []
	isAccept (undefined :: AM2) []
	*Main> isAccept (undefined :: AM2) [O]
	isAccept (undefined :: AM2) [O]
	*Main> isAccept (undefined :: AM2) [I]
	isAccept (undefined :: AM2) [I]
	*Main> isAccept (undefined :: AM2) [I, I, O]
	isAccept (undefined :: AM2) [I, I, O]
	*Main> isAccept (undefined :: AM2) [I, I, O, I]
	isAccept (undefined :: AM2) [I, I, O, I]

オートマトン(まとめ)
--------------------

* AMStateは以下の性質をクラスにしたもの
	- ある型がオートマトンの状態である
* この例で見たように型クラスは
	- 「性質」であると考えられる、だけでなく
	- 仕様と実装を分離するシステムとも考えられる
* step, start, acceptという関数を持つという「仕様」
* それらの「実装」はインスタンス宣言のなかで作られる
* 「AMState q =>」は
	- qがAMStateクラスの仕様を満たすことを保証する

まとめ
------

* 型クラスとは型の持つ「性質」を表現したもの
* 「性質」はその型を扱う関数で表現される
* 「性質」を「仕様」と呼ぶこともできる
* クラス宣言ではクラス関数の型を定義する
* インスタンス宣言ではクラス関数の実装を作成する
* 「[型クラス名] => a」とすることで
	- aを与えられた型クラスのインスタンスに制限できる
	- その型クラスのクラス関数の存在が保証される

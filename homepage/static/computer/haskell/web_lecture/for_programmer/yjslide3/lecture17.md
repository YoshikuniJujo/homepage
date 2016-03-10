第17回 モナドの復習と演習
=========================

はじめに
--------

* 前回はモナドについて学んだ
* すこし難しかったかもしれない
* 今回はもうすこし簡単な例を見る
* また演習問題を解くことで理解することを試みよう

モジュールシステム
------------------

* 今まではモジュールについてあまり意識してこなかった
* しかし、すでに2つのモジュールに触れている
	- Prelude: 基本的な関数が定義されているモジュール
	- Main: モジュール宣言を省略した場合のデフォルト
* モジュール宣言は以下の形式となる
	module [モジュール名] ([エクスポートリスト]) where
* エクスポートリストは名前を','で分けたリスト
* 値構築子のエクスポートは特別な形となる
	[型名]([値構築子名])
* data Foo = BarのようなときのBarをエクスポートするには
	- module Baz (Foo(Bar)) whereのようにする

ライオンの檻
------------

* 檻に入れたライオンをエクスポートするモジュールを作る
* ライオンは状態Hungry, Normal, Fullを持つ
* play関数でライオンは空腹の方向に変化し
* feed関数でライオンは満腹の方向に変化する
* ライオンを操作するときのみ檻から出し
	- 操作後は絶対に檻の外にいてはならない
* lectures/lecture17ディレクトリを作りLion.hsを作ろう

ライオンの檻
------------

* ライオンの持つ状態を定義しよう
	data State = Hungry | Normal | Full deriving Show
* ライオンは名前と状態を持つことにする
	type Name = String
	data Lion = Lion Name State deriving Show
* これらをLion.hsに書き込もう

ライオンの檻
------------

* 檻を定義してみよう
	newtype Caged a = Caged a deriving Show
* はじめのCagedは型構築子で2つめのCagedは値構築子
* Caged aはa型の値をひとつ取る値構築子Cagedで作れる
* このCagedをMonadクラスのインスタンスにしてみる
	instance Monad Caged where
		return = Caged
		Caged x >>= f = f x
* これらをLion.hsに書き込もう

ライオンの檻
------------

* ライオンを生み出す関数を作ろう
* ライオンは檻のなかに生まれることにする
	lion :: Name -> Caged Lion
	lion n = Caged $ Lion n Hungry
* ライオンに餌を与える関数
	feed :: Lion -> Lion
	feed (Lion n Hungry) = Lion n Normal
	feed (Lion n _) = Lion n Full
* これらをLion.hsに書き込もう

ライオンの檻
------------

* ライオンと遊ぶ関数
	play :: Lion -> Lion
	play (Lion n Full) = Lion n Normal
	play (Lion n _) = Lion n Hungry
* これをLion.hsに書き込もう

ライオンの檻
------------

* コードの先頭にモジュール宣言をつけよう
* モジュール名はLionとする
* 使用する型はLionとCagedなのでそれはエクスポートする
* 値構築子のLionをエクスポートすると
	- 檻の外でライオンを生み出すことができてしまう
	- それは危険なので値構築子Lionはエクスポートしない
* 檻のなかでライオンを生み出すlionをエクスポートする
* ライオンを扱う関数feed, playもエクスポートする
* よってモジュール宣言は以下のようになる
	module Lion (Lion, Caged, lion, feed, play) where
* これをLion.hsに書き込もう

ライオンの檻
------------

* 試してみよう
	% ghci Lion.hs
	*Lion> Lion \"danger\" Hungry
		itext t 1 $ show $ Lion "danger" Hungry
		- 危ない!ライオンが檻の外にいる
		- 値構築子Lionはエクスポートしていないはず
* モジュール名の前にある'*'がポイント
* この'*'はそのモジュールのなかにいますよ、という意味
* つまりライオンの生産地(多分アフリカ)にいるので
	- 檻の外にライオンがいてもおかしくない

ライオンの檻
------------

* 文明社会のなかでライオンと戯れたい
* Lionモジュールの外にいてLionモジュールをimportしたい
* ghciでは以下のようにする
	*Lion> :m Lion
	Prelude Lion> 
* PreludeとLionモジュールがエクスポートする関数の使える
	どこでもない場所にいることになる

ライオンの檻
------------

	Prelude Lion> lion "Simba"
		itext t 0 $ show $ lion "Simba"
	Prelude Lion> let simba = it
	Prelude Lion> feed simba
	... Couldn't match ... `Lion' with ... `Caged Lion' ...
	Prelude Lion> simba >>= feed
	... Couldn't match type `Lion' with `Caged b0' ...
	Prelude Lion> simba >>= return . feed
		itext t 0 $ show $ simba >>= return . feed, \t -> do

* 餌を与えた後はちゃんと檻にもどしてあげる必要がある

ライオンの檻(まとめ)
--------------------

* 檻に入れたライオンを輸出するモジュールを作った
* モナド関数では
	- 何かを檻に入れることはできる
	- 檻から一時的に出すことはできる
	- しかし、檻から出しっぱなしにすることはできない
* モジュールを使って内部構造を隠蔽することができる
* CagedやLionの値構築子を輸出していないので
* 内部構造の変更は安全

計算のログ
----------

* モナドとは単なる形式である
* 以下の型の関数が存在しモナド則を満たせばすべてモナド
	m a -> (a -> m b) -> m b
	a -> m a
* 以下のように書いても同じこと
	(a -> m b) -> (b -> m c) -> (a -> m c)
	(a -> b) -> (a -> m b)
* モナドという性質を共有していてもその中身は様々
* 今回の演習では計算のログを取るモナドを組み立ててみよう

計算のログ
----------

* 計算をしながら計算のログ(記録)を取っていくモナドを作る
* ログは文字列のリストとしよう
* 例えば以下の関数があり
	toCode :: Char -> Logger Int
* toCode 'c'は
	- ログとして[\"toCode 'c'\"]を持ち
	- 計算の結果として99を持つ
* つまり、Logger型は文字列のリストと結果の値を持つ
* 演習17-1. Logger型を定義してみよう
	(1分)

計算のログ
----------

* できただろうか?
* できなくても大丈夫
* 大切なのは「頭を悩ませた」ということ
* 自分で考えたあとの解説は記憶に残りやすいし
* 以下の項目について理解する助けになる
	- 「どうしてそうなのか」
	- 「どうしてそうじゃないのか」
* 「理解」とは段階的なもの
	- 何度も塗ることですこしずつ濃くしていけば良い
	- 演者自身もHaskellの理解を日々濃くしている

計算のログ
----------

* Logger型の定義は
	data Logger a = Logger [String] a
* 解答は同じだっただろうか
* 答えはひとつではない
* 以下の解も正解とする
	newtype Logger a = Logger ([String], a)
	data Logger a = Logger ([String], a)
	type Logger a = ([String], a)
* もちろん[String]とaの順番が逆でも正解

計算のログ
----------

* 今後の流れのために他の答えを出した人も以下を使おう
	data Logger a = Logger [String] a deriving Show
* これをlogger.hsに書き込もう
* ログを残しつつ文字コードを求める関数
	toCode :: Char -> Logger Int
* 以下のような値を返すものをするとする
	toCode 'c'
	arrowIText t 1 "Logger [\"toCode 'c'\"] 99
* 演習17-2. toCodeを定義しよう
	(import Data.Char (ord)が必要)
	(1分)

計算のログ
----------

* 解答は以下のようになる
	toCode :: Char -> Logger Int
	toCode c = Logger (\"toCode \" ++ show c) (ord c)
* 以下とあわせてlogger.hsに書き込もう
	import Data.Char (ord)
* 試してみる
	Prelude Lion> :load logger.hs
	*Main> toCode 'c'
		itext t 1 $ show $ toCode 'c'

計算のログ
----------

* 次に以下の関数を作ろうと思うのだが
	double :: Int -> Logger Int
	double 3
	arrowIText t 1 "Logger [\"double 3\"] 6
* その前に文字列をログにする関数を書こう
* 以下のような型になる
	tell :: String -> Logger ()
* ログのほうだけを扱う関数なので
	Logger aのaの部分を()で埋めている
* 演習17-3. tellを定義せよ
	(1分)

計算のログ
----------

* 以下のようになる
	tell :: String -> Logger ()
	tell l = Logger [l] ()
* これをlogger.hsに書き込もう
* これを使ってtoCodeを再定義したい
* さっきのtoCodeの定義を以下のように書き換えよう
	toCode c =
		tell (\"toCode \" ++ show c) >> return (ord c)
* toCodeを以下の2つの部分に分けて構築している
	- ログを追加
	- 文字コードを返す

計算のログ
----------

* 新しいtoCodeの定義はまだ使えない
* LoggerをMonadクラスのインスタンスにする必要がある
* 以下の関数を定義する必要がある
	return :: a -> Logger a
	(>>=) :: Logger a -> (a -> Logger b) -> Logger b
* まずは簡単なほうから
* returnは「何もせずに」値を包み込む関数
* Loggerについて言えば「ログを変化させずに」ということ
	- ログを空にしておけば良い
* 演習17-4. Loggerのreturnを定義せよ
	(1分)

計算のログ
----------

* 以下のようになる
	return x = Logger [] x
* これは以下のようにできる
	return = Logger []
* これはMonadクラスのクラス関数なので
	instance Monad Logger where
		return = Logger []
* これをlogger.hsに書き込もう

計算のログ
----------

* 試してみる
	*Main> :reload
	((>>=)が定義されていないというWarningが出る)
	*Main> return 8 :: Logger Int
		itext t 1 $ show $ (return 8 :: Logger Int), \t -> do
* 問題なく定義できているようだ

計算のログ
----------

* それでは(>>=)の定義に移ろう
	(>>=) :: Logger a -> (a -> Logger b) -> Logger b
* この関数に何をして欲しいか考える
	- 第一引数のa型の値を第二引数である関数にわたして
	- 出てきた結果について
		ログのほうは第一引数のログに追加し
		b型の値のほうは結果の値とする
* 演習17-5. Loggerの(>>=)を定義せよ
	(2分)

計算のログ
----------

* 難しかったかもしれない
* 順を追って見ていこう
* まずは引数のパターンマッチの部分を作ろう
	(>>=) :: Logger a -> (a -> Logger b) -> Logger b
* 第一引数のLogger aは中身のログと値を使う
* 第二引数は関数なので分解できない
	Logger l x >>= f = ...
* 第一引数の「値」にfを適用した結果が必要なので
	Logger l x >>= f = ... f x ...

計算のログ
----------

* 型とできた部分までを再掲する
	(>>=) :: Logger a -> (a -> Logger b) -> Logger b
	Logger l x >>= f = ... f x ...
* f xの結果のログと値を別々に使うのでパターンマッチする
	Logger l x >>= f = let Logger l' x' = f x in ...
* 新しいログは古いログに追加し新しい値はそのまま返すので
	Logger l x >>= f =
	itext t 1.8 "let Logger l' x' = f x in Logger (l ++ l') x'

計算のログ
----------

* logger.hsのインスタンス宣言に追加しよう
	instance Monad Logger where
	return = Logger []
	Logger l x >>= f =
	itext t 1.8 "let Logger l' x' = f x in Logger (l ++ l') x'
* これで前に再定義したtoCodeが動くようになる
	toCode c =
		tell (\"toCode \" ++ show c) >> return (ord c)
* 試してみる
	*Main> :reload
	*Main> toCode 'c'
	itext t 1 $ show $ toCode' 'c'

計算のログ
----------

* 次に以下のように整数を2倍するdoubleを考える
	double :: Int -> Logger Int
	double 8
	arrowIText t 1 $ show $ double 8, \t -> do
* これもtoCodeと同じように定義できる
* 演習17-6. doubleを定義せよ
	(1分)

計算のログ
----------

* 以下のようになる。logger.hsに書き込もう
	double :: Int -> Logger Int
	double n =
		tell (\"double \" ++ show n) >> return (n * 2)
* toCodeとdoubleを使えば
	- ログを記録しながら
	- 文字コードを2倍する関数toCodeDoubleが作れる
* 演習17-7. toCodeDoubleを定義せよ
	(1分)

計算のログ
----------

* 以下のようになる
	toCodeDouble :: Char -> Logger Int
	toCodeDouble c = toCode c >>= double
* logger.hsに書き込み、試してみる
	*Main> :reload
	*Main> toCodeDouble 'c'
	itext t 1 $ show $ toCodeDouble 'c'

計算のログ(まとめ)
------------------

* モナドとは型mについて
	- (a -> m b)型の関数を次々につなげられるという性質
* 中身は何であれその性質を満たせばモナドである
* 結果とその過程を保存するモナドLoggerを作った
	data Logger a = Logger [String] a
* Monadクラスへのインスタンス宣言は以下のようになる
	instance Monad Logger where
		return = Logger []
		Logger l x >>= f = let (l', x') = f x in
	itext t 3 "Logger (l ++ l') x'
* 関数適用の裏でログを結合している

まとめ
------

* ライオンをいれておく檻であるCaged型を作った
* Lion型やCaged型の値構築子を隠蔽することで
	- 檻の外でライオンが作れないようにし
	- 檻からライオンを出しっぱなしにできないようにした
* モナド関数のみを使うことで
	- 檻から出したライオンを檻にもどすことを強制できる
* 演習では計算のログを保存するモナドを作ってみた
* このモナドでは表の計算と裏でのログの結合とが行われる

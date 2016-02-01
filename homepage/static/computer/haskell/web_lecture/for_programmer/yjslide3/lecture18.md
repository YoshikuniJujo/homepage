第18回 IOモナド
===============

はじめに
--------

* Haskellは参照透過性を持つ言語である
* 多くの言語では以下のような形で入出力を扱う
	- 関数が「評価」されるタイミングで入出力を行い
	- 関数の返り値として入力を返す
* 関数の返り値として入力を返すと参照透過性が破壊される
* 「評価」のタイミングで入出力を行うと
	- 「評価」の順がプログラムの意味に影響を与える

はじめに2
---------

* どうすればいいのだろう?
	- 入出力を行う機械という値を作れば良い
	- 数値型は対話環境で評価するとその値を表示する
	- 文字型も対話環境で評価するとその値を表示する
	- 機械型は対話環境で評価するとその動作を行う
* 機械型を数値型と同じように基本的な型として持てば良い

Machine
-------

* たとえばMachine型という型があったとする
* Machine型の値としてputHelloとputWorldがあり
	- それぞれ\"Hello\"と\"World\"を表示するとする
		putHello :: Machine
		putWorld :: Machine
* Machine型の値をつなぐ関数nextがあると
	next :: Machine -> Machine -> Machine
* \"HelloWorld\"と表示する関数は以下のように書ける
	putHelloWorld :: Machine
	putHelloWorld = putHello `next` putWorld"

Machine2
--------

* 出力についてはMachine型でうまくいく
* つまり「出力」というものは本質的に
	- 「これやって」
	- 「次にこれやって」
* それの連続なので。
* 入力がからむとこれはうまくいかなくなる
* 入力値を次の機械にわたす必要が出てくるからだ
* つまり機械のあいだで値をわたす仕組みが必要だ
* ひとつめの機械からふたつめの機械に値を渡す関数
	(>>>) :: Machine -> Machine -> Machine

Machine3
--------

* 以下の機械があるとする
	- getLine: 入力を一行読み、次の機械に渡す機械
	- putLine: 渡された値を表示する機械
* 読み込んだ行を表示する機械は以下のように作れる
	getLine >>> putLine"

Machine4
--------

* しかしこのやりかたには問題がある
* 以下の機械があるとする
	- getInt: 入力を読み、数に変換し次の機械に渡す機械
* そして次のようにすると
	getInt >>> putLine
* putLineは文字列が来ることを期待しているので
	- 数を渡されると予測出来ない動作をするだろう

IOMcn
-----
* つまり型の不一致が生じる可能性がある
* 静的型付け言語であるHaskellでは
	- 型の不一致は型チェックの段階で検出したい
* Machine型に渡される値と渡す値の型を含めれば良い
* これをIOMcn型としよう
	IOMcn a b

IOMcn2
------

* 今まで出てきた機械の型は以下のようになる
	putHello, putWorld :: IOMcn () ()
	getLine :: IOMcn () String
	getInt :: IOMcn () Int
	putLine :: IOMcn String ()
* これらをつなぐ関数は以下のような型となる
	(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c

IOMcn3
------

* 例えば以下のようなつなぎかたは正当
	getLine >>> putLine
* この場合それぞれの型は以下のようになる
	getLine :: IOMcn () String
	putLine :: IOMcn String ()
	(>>>) :: IOMcn () String ->"
		IOMcn String () -> IOMcn () ()
* つないだ結果の型は
	getLine >>> putLine :: IOMcn () ()

IOMcn4
------

* 試してみよう
* lectures/lecture18/IOMcn.hsが用意してあるので
	% ghci IOMcn.hs
	*IOMcn> runIOMcn $ getLine >>> putLine
		-- 何か適当に入力し改行する、ここでは"hello"
	hello
	hello
* 入力した文字列を表示している
* ここでは、runIOMcnで機械を動かしていると考える

IOMcn5
------

* しかし、以下のつなぎかたは型の不適合となる
	getInt >>> putLine
* (>>>)の型を再掲する
	(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
* (>>>)のbの型が
	- getIntからはIntであることを要求され
	- putLineからはStringであることを要求される
* 結果として型エラーとなる

おかしな型の値が機械に渡されることはない

IOMcn6
------

* 試してみよう
	*IOMcn> runIOMcn $ getInt >>> putLine
	...
	Couldn't match type `[Char]' with `Int'
	...
* 確かに、型エラーとなる"

IOMcn7
------

* (>>>)を使えば次々と機械をつないでいくことができる
	m1 >>> m2 >>> m3 >>> m4 >>> ...
* 途中に普通の関数をはさみたいこともある
	- 入力された文字列を逆にして表示したい等々
* 関数を機械に変換する関数が必要になる
	arr :: (a -> b) -> IOMcn a b
* これを使うと入力を逆順にして表示は
	getLine >>> arr reverse >>> putLine
* arr reverseは
	- 文字列を受け取り
	- それを逆順にして次の機械に渡す機械

IOMcn8
------

* 試してみよう
	*IOMcn> runIOMcn $ getLine >>> arr reverse >>> putLine
	-- \"hello\"を入力してみよう
	hello
	olleh
* 入力された文字列が逆順で表示された

IOMcn9
------

* 関数がIOMcnに変換できるということは
	- 普通の値を機械に流し込むことができるということ
* たとえば\"Hello\"を機械に流し込むには以下のようにする
	arr (const \"Hello\") >>> putLine
* 引き数を無視し\"Hello\"を返す関数を機械に変換し
	- その機械と機械putLineとをつないだ
* つまりputHello, putWorldは以下のように定義できる
	putHello = arr (const \"Hello\") >>> putLine
	putWorld = arr (const \"World\") >>> putLine

IOMcn10
-------

* 試してみよう
	*IOMcn> runIOMcn $ arr (const \"Hello\") >>> putLine
	Hello
	*IOMcn> runIOMcn $ arr (const \"World\") >>> putLine
	World
* 引数を無視して\"Hello\"を返す関数(const \"Hello\")
* これを機械に変換
	arr (const \"Hello\") :: IOMcn () String
* これをputLineにつないでいる

IOMcn11
-------

* hello.hsを作って以下を書き込もう
	import IOMcn
	putHello, putWorld :: IOMcn () ()
	putHello = arr (const \"Hello\") >>> putLine
	putWorld = arr (const \"World\") >>> putLine
* 試してみよう
	*IOMcn> :load hello.hs
	*Main> runIOMcn $ putHello >>> putWorld
	Hello
	World

IOMcn12
-------

* 以下のようなあいさつをする機械が作りたい
	- 偶数の秒には\"olleh\"
	- 奇数の秒には\"hello\"
* 今が偶数の秒かどうかを返す機械はあると考えよう
	isEven :: IOMcn () Bool
* 試してみる
	*Main> runIOMcn isEven
		-- その時によってTrueかFalseが表示される
	True
	*Main> runIOMcn isEven
	False

IOMcn13
-------

* Bool値によって以下のどちらかを返す関数を作ろう
	- メッセージを逆順で表示する機械
	- メッセージをそのまま表示する機械
* その関数をmessageという名前で定義する
	message :: Bool -> IOMcn String ()
	message True = arr reverse >>> putLine
	message False = putLine
* これを以下と併わせてgreeting.hsに書き込もう
	import IOMcn

IOMcn14
-------

* 試してみる
	*Main> :load greeting.hs
	*Main> runIOMcn $ arr (const \"hello\") >>> message False
	hello
	*Main> runIOMcn $ arr (const \"hello\") >>> message True
	olleh

IOMcn15
-------

* 以下の機械と関数がある
	isEven :: IOMcn () Bool
	message :: Bool -> IOMcn String ()
* arrと>>>を使って組み合わせて以下の動作の機械を作る
	- 偶数の秒には\"hello\"を逆順で表示し
	- 奇数の秒には\"hello\"を表示する
* 渡されたBool値を受け取るにはmessage関数を機械にする
	arr message :: IOMcn Bool (IOMcn String ())
* これとisEvenをつなげると
	isEven >>> arr message :: IOMcn () (IOMcn String ())

IOMcn16
-------

* 実際に型を見てみよう
	*Main> :t message
	message :: Bool -> IOMcn String ()
	*Main> :t arr message
	arr message :: IOMcn Bool (IOMcn String ())
	*Main> :t isEven >>> arr message
	isEven >>> arr message :: IOMcn () (IOMcn String ())

IOMcn17
-------

* isEven >>> arr messageの型を見ると
	IOMcn () (IOMcn String ())
* 機械を渡す機械ができてしまっている
* IOMcn String ()を動かすにはStringを渡す必要がある
* IOMcn String ()にStringを渡す機械が必要
* より一般的にはIOMcn a bにaを渡す機械が必要になる
	app :: IOMcn (IOMcn a b, a) b
* (「aを受け取りbを渡す機械」とa)を受け取りbを渡す機械
* appを使うには以下の型の機械を作る必要がある
	IOMcn () (IOMcn String (), String)

IOMcn18
-------

* 以下の型の機械と関数がある
	isEven :: IOMcn () Bool
	message :: Bool -> IOMcn String ()
* 組み合わせるための道具には以下のものがある
	arr :: (a -> b) -> IOMcn a b
	(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
	app :: IOMcn (IOMcn a b, a) b
* 今、作りたい機械の型は
	IOMcn () (IOMcn String (), String)
* この型の機械は以下の型の関数とisEvenをつなげばできる
	IOMcn Bool (IOMcn String (), String)

IOMcn19
-------

* この型の関数を作るには"
	IOMcn Bool (IOMcn String (), String)
* 以下の型の関数にarrを適用すれば良い
	Bool -> (IOMcn String (), String)
* messageを使えばこの型の関数はすぐに作れる
	sayHello :: Bool -> (IOMcn String (), String)
	sayHello b = (message b, "hello")
* 関数sayHelloをgreeting.hsに書き込もう

IOMcn20
-------

* 対話環境で求める機械を組み立ててみる
	*Main> :reload
	*Main> :t arr sayHello
	arr sayHello :: IOMcn Bool (IOMcn String (), String)
	*Main> :t isEven >>> arr sayHello
	isEven >>> arr sayHello ::"
	IOMcn () (IOMcn String (), String)
	*Main> :t isEven >>> arr sayHello >>> app
	isEven >>> arr sayHello >>> app :: IOMcn () ()
	*Main> runIOMcn $ isEven >>> arr sayHello >>> app
	olleh
		-- 時間によってhelloまたはolleh"

IOMcn21
-------

* まとめると以下のようになる
	sayHello :: Bool -> (IOMcn String (), String)
	arr sayHello :: IOMcn Bool (IOMcn String (), String)
	isEven >>> arr sayHello ::"
		IOMcn () (IOMcn String (), String)
	isEven >>> arr sayHello >>> app :: IOMcn () ()"

IOMcn22
-------

* 求める関数greetingは
	greeting :: IOMcn () ()
	greeting = isEven >>> arr sayHello >>> app
* それぞれの機械を説明すると
	- Boolを渡す機械
	- Boolを受け取り"
		(「文字列を受け取る機械」と文字列)を渡す機械
	- (「文字列を受け取る機械M」と文字列S)を受け取り"
		機械Mに文字列Sを渡す機械
* 関数greetingの定義をgreeting.hsに書き込もう

IOMcn23
--------

* 試してみる
	*Main> :reload
	*Main> runIOMcn greeting
	hello
	*Main> runIOMcn greeting
	hello
	*Main> runIOMcn greeting
	olleh

IOMcn(まとめ)
-------------

* 多くの言語ではIOは以下のように行われる
	- 関数の評価のタイミングで入出力動作を行い
	- 入力値は関数の返り値として受け取れる
* 参照透過性と遅延評価の面から上記の方法は望ましくない
* むしろIOを行う機械を組み立てていくことを考える
* 機械が受け取る値の型と渡す値の型を指定すると良い
* 以下の型とそれに対する関数や機械を用意しておくと良い
	IOMcn a b
	(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
	arr :: (a -> b) -> IOMcn a b
	app :: IOMcn (IOMcn a b, a) b

IO
--

* IOMcnはもっとスマートにすることができる
* 以下の型を比較してみる
	IOMcn a b
	a -> IOMcn () b
* これらの型が相互に変換可能であることを示そう

IO2
---

* まずは(IOMcn a b)から(a -> IOMcn () b)を作る関数
	outArg :: IOMcn a b -> a -> IOMcn () b
	outArg iom = \\x -> arr (const x) >>> iom
* 型aの値xがあれば以下の機械が作れる
	arr (const x) :: IOMcn () a
* これと(iom :: IOMcn a b)をつなげればIOMcn () bは作れる
* 「(IOMcn a b)とaからIOMcn () bを作れる」は以下と同値
	- 「(IOMcn a b)から(a -> IOMcn () b)を作れる」

IO3
---

* 逆に(a -> IOMcn () b)から(IOMcn a b)が作れる
	inArg :: (a -> IOMcn () b) -> IOMcn a b
	inArg f = arr (\\x -> (f x, ())) >>> app
* (f :: a -> IOMcn () b)があれば以下の関数が作れる
	\x -> (f x, ()) :: a -> (IOMcn () b, ())
* ここから以下の機械が作れる
	arr (\\x -> (f x, ())) :: IOMcn a (IOMcn () b, ())
* さらに機械appをつなげば良い
	arr (\\x -> (f x, ())) >>> app :: IOMcn a b

IO4
---

* つまり以下の2つの型は同じものであると考えられる
	IOMcn a b
	a -> IOMcn () b
* aを受け取ってbを渡す機械を
	- aの値によって「bを渡す機械」を選ぶ関数に変換可能
* IOMcn a bの形の関数をa -> IOMcn () bの形に統一し
	type IO = IOMcn ()としてみよう

IO5
---

* すると以下の関数のペアを
	(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
	arr :: (a -> b) -> IOMcn a b
* 以下の形に変えることができる
	(>=>) :: (a -> IO b) -> (b -> IO c) -> (a -> IO c)
	arr' :: (a -> b) -> (a -> IO b)
* これはモナド関数だ
	(>>=) :: IO a -> (a -> IO b) -> IO b
	return :: a -> IO a

IO6
---

* aを受け取りbを渡す機械を以下の関数に変換する
	- aを引数として取り「bを渡す機械」を返す関数
* そうすることによりIOを行う機械をモナドとして扱える
* 以下の関数を
	(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
	arr :: (a -> b) -> IOMcn a b
* 以下の関数で置き換えられる
	(>>=) :: IO a -> (a -> IO b) -> IO b
	return :: a -> IO a

IO7
---

* 機械に値を渡す機械については
	app :: IOMcn (IOMcn a b, a) b
	(a -> IO b, a) -> IO b
* これは単に関数適用に置き換えられるので不要になる

IO8
---

* 試してみよう
* 文字列を改行をつけて表示する関数
	putStrLn :: String -> IO ()
* 対話環境で試してみる
	*Main> :load
	OK, modules loaded: none.
	Prelude> putStrLn \"Hello\"
	Hello
* putStrLnは形としては文字列によって機械を選ぶ関数
* しかし、その中身は文字列を受け取る機械

IO9
---

* runIOMcnに当たるものがない
	- 対話環境で評価された機械は暗黙のうちに「実行」
* これは以下と対照的
	- 対話環境で評価された数値は暗黙のうちに「表示」
* 見方を変えると
	- 数値を表示するprintという機械がある
	- 対話環境で数値が評価された場合
	- 暗黙のうちにprintという機械に渡されて
	- その機械が「実行」される

IO10
----

* 入力についても試してみよう
* キーボードからの入力を一行読み込む関数
	getLine :: IO String
* 対話環境で試してみる
	Prelude> getLine
		-- 適当な文字列を入力しよう
	hello
	"hello"

IO11
----

* 前にライオンの檻について見た
	- モナド関数はモナドから外に値が出ることを許さない
* Haskellでは「状態変化」はIOの外では起こらない
* IOを実行する以外の場所では参照透過性が保たれている
* IOの中の値を取り出すことはできる
	- しかし、その後に「しまう」必要がある

IO12
----

* IOはモナドなのでMonadクラスのインスタンスになっている
	(>>=) :: IO a -> (a -> IO b) -> IO b
	return :: a -> IO a
* もちろんIOモナドでもdo記法が使える
	some :: IO ()
	some = do
		str <- getLine
		putStrLn str
* 手続き型言語のような外見になる

IO(まとめ)
----------

* IOMcn a bをa -> IOMcn () bにすることができた
* type IO = IOMcn ()とすると
	IOをモナドとして扱うことができる
* IOの中に入った値はIOの外に取り出せない
	参照透過性が保たれる
* IOをつないでいくのには
	- (>>)や(>>=)が使える
	- do記法を使えば手続き型言語のような外見になる

まとめ
------

* IOモナドを説明するためにその前段階として
	IOMcnという型を導入した
* IOMcnに必要な関数は以下のようになる
	(>>>) :: IOMcn a b -> IOMcn b c -> IOMcn a c
	arr :: (a -> b) -> IOMcn a b
	app :: IOMcn (IOMcn a b, a) b
* IOMcnを変換することでIOが導き出せる
	(>>=) :: IO a -> (a -> IO b) -> IO b
	return :: a -> IO a
* Haskellでは入出力にIOモナドを使っている

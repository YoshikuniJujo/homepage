第II部 4. stackの導入
===================

stackとは
---------

GHCではライブラリやアプリケーションはパッケージにまとめられる。
パッケージはcabalというツールで管理される。
パッケージがすくなく楽園的だった時代はそれでよかった。
パッケージがふえていきそれらのパッケージのバージョンも
さまざまにアップデートされた。
パッケージどうしの依存関係も複雑化していった。

あるパッケージBがパッケージAのバージョン1.0に依存している。
バージョン2.0では動かない。
逆にパッケージCはパッケージAのバージョン2.0に依存していて
バージョン1.0では動かない。

おなじひとつのシステムでパッケージBとCを同時にいれるにはどうすればいいのだろうか。
パッケージBのためにAのバージョン1.0をインストールした環境と
パッケージCのためにAのバージョン2.0をインストールした環境とをつくり、
それぞれ独立して運用する必要がある。
stack以前ではそれを手作業で行った。
つかうパッケージがふえていくにしたがってこのやりかたは手に負えなくなる。
問題はパッケージどうしの依存関係にとどまらない。
HaskellのコンパイラであるGHCのバージョンもパッケージごとに用意しなければならない。

stackはそのような専用の環境を自動的につくってくれる。
GHCを直接導入するのではなく、
まずはstackを導入したうえでそれをつかってGHCを導入するほうが
問題なくHaskell環境をつくることができる。

stackの導入
-----------

[http://docs.haskellstack.org](http://docs.haskellstack.org "http://docs.haskellstack.org")参照

### Windows

コマンドstackを走らせるまえに環境変数STACK\_ROOTをセットしておくとよい。

	set STACK_ROOT=c:\stack_root

のようにする(らしい、あとでWindows PCでチェックする必要がある)。
Windowsではファイルパスの長さが260文字までに制限されている。
STACK\_ROOTを短くしておくことでその制限にひっかかることを回避できる。

#### メモ

以下を調べる必要がある。

* STACK\_ROOT変数はインストーラが設定してくれるかどうか
* STACK\_ROOT変数に指定されたディレクトリは自分で用意する必要があるかどうか

[「Windowsでstackをつかううえでの問題点」](
	https://www.fpcomplete.com/blog/2015/08/stack-ghc-windows
	"Windowsでstackをつかううえでの問題点")を読む。

#### インストーラをつかう

32ビット用と64ビット用のインストーラが用意されている。

[http://www.stackage.org/stack/windows-i386-installer](
	http://www.stackage.org/stack/windows-i386-installer
	"Windows 32ビット用インストーラ")
[http://www.stackage.org/stack/windows-x86_64-installer](
	http://www.stackage.org/stack/windows-x86_64-installer
	"Windows 64ビット用インストーラ")

#### 実行可能ファイルを直接導入する

実行可能ファイルを直接導入することもできる。

[http://www.stackage.org/stack/windows-i386](http://www.stackage.org/stack/windows-i386 "Windows 32ビット用実行可能ファイル")
[http://www.stackage.org/stack/windows-x86_64](http://www.stackage.org/stack/windows-x86_64 "Windows 64ビット用実行可能ファイル")

システムにあわせてどちらかの圧縮ファイルをダウンロードし解凍する。
stack.exeを%APPDATA%\local\binにコピーする。
またこのディレクトリにパスを通しておこう。
これでターミナルからstackコマンドが実行できる。

### Mac OS X

#### Homebrewをつかう

Homebrewをつかっているユーザーであれば

	brew install haskell-stack

とする。
ビルドずみのバイナリをインストールするはずだ。
あとで

	stack setup

としたときに

	configure: error: cannot run C compiled programs

のようなエラーがでたときには

	xcode-select --install

とする。

#### 実行可能ファイルを直接導入する

下記のリンクから圧縮ファイルをダウンロードする。

[https://www.stackage.org/stack/osx-x86_64](
	https://www.stackage.org/stack/osx-x86_64)

展開し実行可能ファイルstackをみつける。
ホームディレクトリに.local/binディレクトリを作成しそこにstackをコピーする。
$HOME/.local/binにパスを通しておこう。

### Debian

64ビットのシステムでのインストールを説明する。
32ビットのシステムやこのやりかたでうまくいかないときにはLinux一般のやりかたを
参照すること。

#### FP CompleteのGnuPG鍵を入手

FP Complete社のGnuPG鍵を入手する。

	sudo apt-key adv --keyserver keyserevr.ubuntu.com -- recv-keys 575159689BDFB442

#### リポジトリの追加

適切なリポジトリを追加する。
Debian 8(amd64)では

	echo 'deb http://download.fpcomplete.com/debian jessie main' | sudo tee /etc/apt/sources.list.d/fpco.list

とし、Debian 7(amd64)では

	echo 'deb http://download.fpcomplete.com/debian wheezy main' | sudo tee /etc/apt/sources.list.d/fpco.list

とする。

#### aptの更新と導入

パッケージのデータベースを更新しstackを導入する。

	sudo apt-get update && sudo apt-get install stack -y

### Ubuntu

64ビットのシステムでのインストールを説明する。
32ビットのシステムやこのやりかたでうまくいかないときにはLinux一般のやりかたを
参照すること。

#### FP Complete GnuPG鍵を入手

FP Complete社のGnuPG鍵を入手する。

	sudo apt-key adv --keyserver keyserver.ubuntu.com --recv-keys 575159689BEFB442

#### 適切なリポジトリの追加

つかっているUbuntuのバージョンを調べる。

	lsb_release -a

Ubuntu 15.10(amd64)ならば

	echo 'deb http://download.fpcomplete.com/ubuntu wily main' | sudo tee /etc/apt/sources.list.d/fpco.list

とする。Ubuntu 15.04(amd64)ならば

	echo 'deb http://download.fpcomplete.com/ubuntu vivid main' | sudo tee /etc/apt/sources.list.d/fpco.list

とする。Ubuntu 14.10(amd64)ならば

	echo 'deb http://download.fpcomplete.com/ubuntu trusty main' | sudo tee /etc/apt/sources.list.d/fpco.list

とする。Ubuntu 12.04(amd64)ならば

	echo 'deb http://download.fpcomplete.com/ubuntu precise main' | sudo tee /etc/apt/sources.list.d/fpco.list

とする。

#### aptの更新とstackの導入

パッケージのデータベースを更新しstackを導入する。

	sudo apt-get update && sudo apt-get install stack -y

### CentOS/Red Hat/Amazon Linux

64ビットのシステムでのインストールを説明する。
32ビットのシステムやこのやりかたでうまくいかないときにはLinux一般のやりかたを
参照すること。

#### 適切なリポジトリの追加

適切なリポジトリを追加する。
CentOS7/RHEL7(x86\_64)では

	curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/7/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

とする。
CentOS6/RHEL6(x86\_64)では

	curl -sSL https://s3.amazonaws.com/download.fpcomplete.com/centos/6/fpco.repo | sudo tee /etc/yum.repos.d/fpco.repo

とする。

#### 導入

stackを導入する。

	sudo yum -y install stack

### Gentoo

Gentooのパッケージシステムによるstackのインストールは現在のところ推奨しない。
stackはそれ自体がパッケージ管理システムだ。
stackがghcの導入などのめんどうをみてくれる。
これをソースコードからビルドするのはあまり適切なやりかたではないと思われる。
依存関係などいろいろとやっかいな問題がある。

Linux一般を参照する。
ひとつ注意することはncursesをUSE=tinfoでビルドしておくことだ。

### Linux一般

64ビット版と32ビット版とがある。
実行可能ファイルを直接導入する。
ディストリビューションによって名前は異なるが

	perl, make, automake, gcc, gmp-devel

などのパッケージがシステムに導入されている必要がある。

#### 圧縮ファイルのダウンロード

システムにあわせて64ビット版または32ビット版をダウンロードする。

[https://www.stackage.org/stack/linux-x86\_64](
	https://www.stackage.org/stack/linux-x86_64)
[https://www.stackage.org/stack/linux-i386](
	https://www.stackage.org/stack/linux-i386)

多くのシステムはlibgmp4(libgmp.so.3)からgmpに更新されたが
CentOS/RHEL/Amazon Linux 6.xなどはまだlibgmp4なのでそれらについては

[https://www.stackage.org/stack/linux-x86\_64-gmp4](
	https://www.stackage.org/stack/linux-x86_64-gmp4)
[https://www.stackage.org/stack/linux-i386-gmp4](
	https://www.stackage.org/stack/linux-i386-gmp4)

のほうをダウンロードする。

#### 実行可能ファイルstackをコピー

圧縮ファイルを拡張し実行可能ファイルstackをコピーする。
ファイル名などはそれぞれにあわせてかえる。

	% tar xvzf stack-1.0.4.3-linux-x86_64.tar.gz
	% mkdir -p ~/.local/bin
	% cp stack-1.0.4.3-linux-x86_64/stack ~/.local/bin/

#### パスを通す

つかっているシェルがbashであれば

	% vi .bashrc
	PATH=$HOME/.local/bin:$PATH

となる。
つかっているシェルがzshであれば

	% vi .zshrc
	path=($HOME/.local/bin(N-/) $path)

となる。

### セットアップ

GHCのインストールは

	% stack setup

とする。

	% stack ghc -- --version

として確認しておこう。
ほかにもいろいろと表示されるがとりあえず

	The Glorious Glasgow Haskell Compilation System, version 7.10.3

と表示されればインストールは成功だ。

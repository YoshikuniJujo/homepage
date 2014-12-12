EAPI=3

CABAL_FEATURES="bin"
inherit haskell-cabal

DESCRIPTION="Not Serious Crypto Tool"
HOMEPAGE="http://skami.iocikun.jp/computer/haskell/coding/nsc.html"
SRC_URI="http://homepage3.nifty.com/salamander/second/portage/distfiles/nsc-0.0.0.1.tar.gz"

LICENSE="BSD3"
SLOT="0"
KEYWORDS="x86 amd64"

DEPEND=">=dev-lang/ghc-6.10
dev-haskell/cabal
<dev-haskell/cryptohash-0.12
<dev-haskell/base64-bytestring-1.1
<dev-haskell/crypto-cipher-types-0.1
<dev-haskell/cipher-aes-0.3
<dev-haskell/monads-tf-0.2
<dev-haskell/cryptohash-0.12
<dev-haskell/base64-bytestring-1.1
<dev-haskell/crypto-cipher-types-0.1
<dev-haskell/cipher-aes-0.3
<dev-haskell/monads-tf-0.2
"

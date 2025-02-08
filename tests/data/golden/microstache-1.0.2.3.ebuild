# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8

# ebuild generated by hackport 0.8.4.0

CABAL_HACKAGE_REVISION=1

CABAL_FEATURES="lib profile haddock hoogle hscolour test-suite"
inherit haskell-cabal

DESCRIPTION="Mustache templates for Haskell"
HOMEPAGE="https://github.com/haskellari/microstache"

LICENSE="BSD"
SLOT="0/${PV}"
KEYWORDS="~amd64"

RDEPEND="
	>=dev-haskell/parsec-3.1.11:=[profile?] <dev-haskell/parsec-3.2
	>=dev-haskell/unordered-containers-0.2.5:=[profile?] <dev-haskell/unordered-containers-0.3
	>=dev-haskell/vector-0.11:=[profile?] <dev-haskell/vector-0.14
	>=dev-lang/ghc-8.8.1:=
	|| (
		( >=dev-haskell/aeson-0.11 <dev-haskell/aeson-1.6 )
		( >=dev-haskell/aeson-2 <dev-haskell/aeson-2.2 )
	)
	dev-haskell/aeson:=[profile?]
	|| (
		( >=dev-haskell/text-1.2.3 <dev-haskell/text-1.3 )
		( >=dev-haskell/text-2.0 <dev-haskell/text-2.1 )
	)
	dev-haskell/text:=[profile?]
"
DEPEND="
	${RDEPEND}
	>=dev-haskell/cabal-3
	test? (
		>=dev-haskell/base-orphans-0.8.7 <dev-haskell/base-orphans-0.9
		>=dev-haskell/tasty-1.4.0.1 <dev-haskell/tasty-1.5
		>=dev-haskell/tasty-hunit-0.10.0.3 <dev-haskell/tasty-hunit-0.11
	)
"

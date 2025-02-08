# Copyright 1999-2023 Gentoo Authors
# Distributed under the terms of the GNU General Public License v2

EAPI=8

# ebuild generated by hackport 0.8.4.0

CABAL_HACKAGE_REVISION=5

CABAL_FEATURES="lib profile haddock hoogle hscolour test-suite"
inherit haskell-cabal

DESCRIPTION="Compatibility package for time"
HOMEPAGE="https://github.com/haskellari/time-compat"

LICENSE="BSD"
SLOT="0/${PV}"
KEYWORDS="~amd64"

RDEPEND="
	>=dev-haskell/base-orphans-0.8.4:=[profile?] <dev-haskell/base-orphans-0.10
	>=dev-haskell/hashable-1.3.2:=[profile?] <dev-haskell/hashable-1.5
	>=dev-lang/ghc-8.8.1:=
"
DEPEND="
	${RDEPEND}
	>=dev-haskell/cabal-3
	test? (
		>=dev-haskell/base-compat-0.10.5 <dev-haskell/base-compat-0.14
		>=dev-haskell/quickcheck-2.13 <dev-haskell/quickcheck-2.15
		>=dev-haskell/tagged-0.8.6 <dev-haskell/tagged-0.9
		>=dev-haskell/tasty-1.2.1 <dev-haskell/tasty-1.5
		>=dev-haskell/tasty-hunit-0.10 <dev-haskell/tasty-hunit-0.11
		>=dev-haskell/tasty-quickcheck-0.10 <dev-haskell/tasty-quickcheck-0.11
		|| (
			( >=dev-haskell/hunit-1.3.1 <dev-haskell/hunit-1.3.2 )
			( >=dev-haskell/hunit-1.6 <dev-haskell/hunit-1.7 )
		)
	)
"

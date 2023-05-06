## v0.8.2.2 (2023-04-29)

v0.8.2.2

Set modern defaults for LocalInfo in Portage.Host (this will help in
the event that askPortageq fails, such as on a clean stage3 chroot)

## v0.8.2.1 (2023-04-02)

v0.8.2.1

Allow Setup.hs to work without cabal-doctest

## v0.8.2.0 (2023-03-03)

v0.8.2.0

Update GHCCore dependency list:

- Mark 'stm' as non-upgradeable, 'parsec' and 'text' as upgradeable
- Update bundled version of 'process'
- Add `ghc-9.2.*` entries
- Remove ancient `<ghc-8.8` entries

## v0.8.1.0 (2023-02-16)

v0.8.1.0

- Revert change that adds dev-haskell/process to ebuilds

## v0.8.0.0 (2022-12-25)

v0.8.0.0

- Switch 'cabal' submodule to new 3.8 branch
- Use new updateAction from cabal-install
- Builds on ghc-9.2
- Make process an upgradeable package

Thanks-to: Miezhiko <Miezhiko@gmail.com>

## v0.7.3.1 (2022-12-07)

Bump to v0.7.3.1

- Support optparse-applicative-0.17

## v0.7.3.0 (2022-08-26)

Add remote-id support for metadata.xml

## v0.7.2.1 (2022-07-23)

Release v0.7.2.1

## v0.7.2 (2022-07-19)

Release v0.7.2

Main change is automated hackage revision
handling when using 'hackport merge'.

## v0.7.1.2 (2022-07-17)

Release version 0.7.1.2

Main user-facing change is the removal of repoman.

## v0.7.1.1 (2022-02-19)

Tag version 0.7.1.1: minor .cabal file fixes

## v0.7.1 (2022-02-19)

Tag version 0.7.1. Main changes include:

- ghc-9.0.2 library detection and bundled library changes
- bumped and patched submodules for compatibility with newer deps
- miscellaneous changes to test-suite, http-only websites list, docs
  and .cabal file.

## v0.7 (2021-07-10)

release 0.7

## v0.6.7 (2020-12-18)

release v0.6.7

## v0.6.6 (2020-07-27)

Release v0.6.6

Changes:

* Add `Hspec` test suite, migrating older `HUnit` tests to `Hspec`.

  There are currently 55 tests validating parsers, `metadata.xml`
  generators, licence converters and more to mitigate against
  unintended regressions

* Add `doctest` test suite to validate code comments.

  Although hackport is not a library, code comments are still
  important. Where we have code examples, let's make sure they are
  correct

* Comment the code in many places

* Refactor code, such as splitting `Merge.hs` between itself and
  `Merge/Utils.hs`, applying some hlint suggestions, and addressing
  some compiler warnings

* Fix a bug in the `Suffix` parser

* Raise `base` lower bound to 4.9 (hackport has not been able to build
  on `base-4.8` for many versions)

* Add GitHub CI for the GHC versions 8.0 -> 8.10 inclusive

* Set licence to GPL-3 in `.cabal` file, which it always was

* Rebase onto `Cabal-3.4` and `hackage-security-0.6.0.1` submodules

* Improve licence handling to more accurately generate the correct
  licence string where possible. This also fixes a regression which
  snuck in after a previous `Cabal` submodule upgrade, which would
  generate invalid GPL licence strings if the package's `.cabal` file
  used a `SPDX` licence identifier such as `GPL-<v>-or-later`

* Automate the addition of new USE flags into an existing
  `metadata.xml`.

  This new functionality alerts the user to the new `USE` flags (if
  any), and overwrites the existing `metadata.xml` with the union of
  the existing and new `USE` flags. This also updates any `USE` flag
  descriptions, as well as any other updated `metadata.xml` elements

* Add `GHC-8.8.4` library detection.

## v0.6.5 (2020-07-01)

Release 0.6.5

## v0.6.4 (2020-01-31)

Release 0.6.4

## v0.6.3 (2020-01-19)

hackport.cabal: release 0.6.3

Signed-off-by: Jack Todaro <jack.todaro@posteo.net>

## v0.6.2 (2020-01-04)

release v0.6.2

## v0.6.1 (2019-10-07)

Release v0.6.1

## v0.6 (2019-02-04)

New in 0.6:

* Builds with GHC 8.6
* Generates EAPI 7-compliant ebuilds
* Generates USE flags in the metadata.xml if the file does not yet exist.

## v0.5.6 (2018-05-30)

release 0.5.6

## v0.5.5 (2018-03-15)

release v0.5.5

## v0.5.4 (2017-09-23)

Release v0.5.4

## v0.5.3 (2017-05-24)

release v0.5.3

## v0.5.2 (2017-02-25)

tag v0.5.2

## v0.5.1 (2016-11-12)

release v0.5.1

## v0.5 (2016-04-20)

release 0.5

## v0.4.7 (2016-02-06)

release 0.4.6

## v0.4.6 (2015-08-12)

release 0.4.6

## v0.4.5 (2015-03-28)

release 0.4.5

## v0.4.4 (2014-09-26)

release v0.4.4

## v0.4.3 (2014-07-24)

release 0.4.3

## v0.4.2 (2014-05-29)

release 0.4.2

## v0.4.1 (2014-05-29)

release 0.4.1

## v0.4 (2014-04-05)

release v0.4

## v0.3.6 (2014-01-07)

tag release v0.3.6

## v0.3.5 (2013-11-28)

hackport.cabal: tag 0.3.5

Signed-off-by: Sergei Trofimovich <slyfox@gentoo.org>

## v0.3.4 (2013-09-14)

release 0.3.4

## v0.3.3 (2013-07-31)

release 0.3.3

## v0.3.2 (2013-01-01)

release v0.3.2

## v0.3.1 (2012-11-28)

release 0.3.1

## v0.3 (2012-10-17)

release 0.3

## v0.2.19 (2012-09-24)

release 0.2.19

## v0.2.18 (2012-05-29)

release 0.2.18

## v0.2.17 (2012-02-29)

release 0.2.17

## v0.2.16 (2012-02-16)

release 0.2.16

## v0.2.15 (2012-02-05)

release 0.2.15

## v0.2.14 (2011-11-28)

release 0.2.13

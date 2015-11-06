Hackport
========

About
-----

Hackport is a utility application for Gentoo Linux to ease the tasks for the
Haskell Project.

The main purpose for Hackport is to interact with Hackage and create
Ebuilds from Cabal packages. It also does handy functions to compare
hackage, the overlay and the portage tree.

Quick start
-----------

1. Build hackport binary by hand (or install it from haskell overlay).
2. Setup hackport database into overlay you plan to merge new ebuilds:

::

    $ mkdir ~/overlays
    $ cd ~/overlays
    $ git clone git://github.com/gentoo-haskell/gentoo-haskell.git
    $ cd gentoo-haskell
    $ hackport update
    $ ls -1 .hackport/
        00-index.tar
        00-index.tar.gz

3. Add your ~/overlays/gentoo-haskell to PORTDIR_OVERLAY in /etc/portage/make.conf.

Done! Now you can `hackport merge <package-name>` to get an ebuild merged to
your overlay!

Features
--------

    'hackport update'
        Update the local copy of hackage's package list. You should run this
        every once in a while to get a more recent copy.

    'hackport list [FILTER]'
        Print packages from hackage, with an optional substring matching.

    'hackport merge <package>'
        Create a Gentoo Linux Ebuild for hackage package named <package>.
        The category defaults to dev-haskell, but is overridden if an older
        version has been merged previously to another category. The category
        can also be overridden with the syntax category/package. Example:

            $ hackport merge x11-wm/xmonad

        Hackport will make an ebuild that uses the haskell-cabal eclass, and
        set the following properties:

        PN (package name)
            Package name converted into lower case
        PV (package version)
            Package version with tags dropped.
        KEYWORDS
            Defaults to ~amd64 ~x86
        CABAL_FEATURES
            Set to "bin" for executables, and "lib haddock profile" for
            libraries. Packages that contains both a binary and library will
            get the union.
        DEPEND
            GHC dependency defaults to >=dev-lang/ghc-6.6.1.
            Cabal dependency is fetched from Cabal field 'Cabal-Version'.
            All other package dependencies are converted into gentoo syntax.
            Range dependencies are flattened and usually needs manual
            tweaking.
        DESCRIPTION
            From Synopsis if it is non-empty, otherwise Description.
        HOMEPAGE
            From Homepage
        SRC_URI
            From package url
        LICENSE 
            From cabal license converted into gentoo licenses
        SLOT
            Defaults to "0"

    'hackport diff [missing|additions|newer|common]'
        Prints a list showing a diff between hackage and the overlay.
        For each package it shows the latest version in both hackage and the
        overlay.


        Optional parameters:
            'all', the default action
                List all packages.
            'missing'
                List packages that exist in hackage but not in the overlay,
                or where the hackage version is more recent.
            'additions'
                List packages only in the overlay, or where the overlay has
                a more recent version.
            'newer'
                List packages where hackage has a more recent version.
            'common'
                List packages where hackage and the overlay has the same
                version.

    'hackport status [toportage]'
        Provides an overview comparing the overlay to the portage tree.
        It will teel you, for each package and version, if the package exist

            - only in the portage tree
            - only in the overlay
            - both in the portage tree and the overlay
            - both in the portage tree and the overlay,
                but the ebuilds are not identical

        Optional parameters:
            '--to-portage'
                Only print packages that are likely to be interesting to
                move to the portage tree.
                It will print packages when they exist in both portage and
                the overlay, and:
                    - the ebuilds differ, or
                    - the overlay has a more recent version

    'hackport make-ebuild <category> <path/to/package.cabal>'
        Generates standalone .ebuild file from .cabal spec and stores result
        to the overlay into <category>/<package>
        Option is useful for not-on-hackage packages and for debug purposes.

-------

    Henning Günther
    Duncan Coutts
    Lennart Kolmodin

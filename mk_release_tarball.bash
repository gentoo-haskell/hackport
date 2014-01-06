#!/usr/bin/env bash

set -e

srcdir=`pwd`
tempdir=`mktemp --directory`
latest_tag=`git tag --list | tail -n 1`
P="hackport-${latest_tag#v}"
tarball_name="${srcdir}/${P}.tar.gz"
(
    cd "${tempdir}"
    echo "building '${tarball_name}' in '${tempdir}'"

    git clone "${srcdir}/" "${P}"
    (
        cd "${P}"
        git reset --hard "${latest_tag}"
        git clean -dfx
        git submodule update --init

        # drop redundant bits
        rm -r -- .git cabal/.git
        # cabal is not able to unpack long tar names
        rm -r -- cabal/Cabal/tests
    )
    tar -czf "${tarball_name}" "${P}"/
)
rm -rf -- "${tempdir}"

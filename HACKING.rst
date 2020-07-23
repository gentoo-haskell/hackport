CONTRIBUTING TO HACKPORT
========================

Introduction
------------

First of all, welcome to ``HackPort`` development!

Hacking on ``HackPort`` should be more-or-less straightforward, but
there are some peculiarities that may cause headaches for new
contributors. This document aims to cover some common pitfalls for new
contributors to ``HackPort``.

Setting up your development repository
--------------------------------------

On GitHub, fork the ``gentoo-haskell/hackport`` repository. If you will be
working on the ``cabal`` submodule, you should also fork the
``gentoo-haskell/cabal`` repository.

The ``HackPort`` source repository contains two git submodules: ``cabal``
and ``hackage-security``. Ensure that these submodules are populated by
cloning the ``HackPort`` repository with the ``--recurse-submodules``
option:

``git clone --recurse-submodules https://github.com/<your-name>/hackport.git``

If you have already cloned the ``HackPort`` source repository without
``--recurse-submodules``, run
``git submodule init && git submodule update`` to populate the submodule
directories. For more information visit
https://git-scm.com/book/en/v2/Git-Tools-Submodules.

Bumping the cabal submodule
---------------------------

The ``cabal`` submodule follows the upstream Cabal source repository,
and adds a handful of local patches designed to allow ``HackPort`` to
use it correctly. Enter the ``cabal/`` directory and run ``git log`` to
see for yourself: the most recent commits will be a dozen or so patches
written by ``HackPort`` contributors over the years to ensure
compatibility between ``HackPort`` and Cabal. Underneath those commits will
be those of Cabal upstream.

Our patches need to be carried over whenever we bump the ``cabal``
submodule to a newer upstream commit. Let’s run through one way of
carrying out this process.

1. Add the upstream Cabal source repository as a remote repository in
   our cabal submodule, i.e.

   ``cd cabal && git remote add upstream https://github.com/haskell/cabal``

   Note that this only needs to be done once.

2. Checkout a new branch within your ``cabal`` submodule, i.e.
   ``git checkout -b <name>``

3. Fetch the latest changes from Cabal upstream (or just the changes you
   need), e.g. \ ``git fetch upstream master``.

4. Rebase onto the latest changes or a given commit, e.g.

   ``git rebase 6e9d6bdc79ecab601d6602d445e9cdcbecfd2591``

   What I usually like to do is browse the upstream Cabal repository on
   GitHub to find a stable point in its commit history, and for that
   I’ll find a commit that is tagged as ‘Cabal-v<version>’. **A WORD OF
   CAUTION:** generally Cabal upstream creates tags in branches
   *outside* of ‘master’, which can cause rebasing headaches in the
   future. If you find yourself looking at a specific commit referenced
   by a git tag for a given Cabal version, check the commit description
   for a comment such as ‘(cherry picked from commit 853414b)’. Commit
   ‘854514b’ in this example is *generally* a commit in the ‘master’
   branch, which is what we want. Rebase onto *that* commit, not the
   commit tagged as ‘Cabal-v<version>’. In this example, do

   ``git rebase 854514b`` and **not** ``git rebase Cabal-v<version>``

   If you *really* want to rebase onto a specific tag in a specific
   branch other than master, you may find that when rebasing to a newer
   upstream Cabal commit in the future you will need to rebase by doing

   ``git rebase --onto <new parent> <old parent>``

   ``<new parent>`` being the new commit to rebase onto and
   ``<old parent>`` being the commit that we were previously based on.

5. Work through the ensuing conflicts. You are likely to come across
   some conflicts between our patches and the changes pulled from Cabal
   upstream. This is normal. Work through the conflicts (about a dozen
   or so) by carrying over our changes into the newer Cabal files that
   we’ve just rebased onto. Usually, it’s a simple matter of ensuring
   that a particular language extension is enabled in a certain file,
   such as adding

   ``{-# LANGUAGE NoMonoLocalBinds #-}``

   to the top of a Cabal source file.

6. Try to compile hackport on top of the updated ``cabal`` submodule:

   ``cabal v2-build``

   This may be the tricky part. There may have been breaking changes
   between upstream Cabal versions, which can cause breakages within
   ``HackPort``.

   If files in the ``cabal`` submodule fail to compile, it’s usually
   related to a language extension that needs to be enabled such as
   ``NoMonoLocalBinds``.

   If files in the ``HackPort`` repository fail to compile, it’s usually to
   do with functions which have been changed or removed in Cabal
   upstream, now reflected in our updated ``cabal`` submodule. You’ll
   need to study the Cabal library documentation for the relevant
   changes, which can be done on Hackage in your web browser.

7. Hopefully, everything now compiles and ``HackPort`` functions
   correctly at run time. Assuming it is decided that these changes
   should be pushed into gentoo-haskell’s ``HackPort`` repository (and
   assuming that you have commit access) ensure that you push both the
   ``cabal`` submodule and the ``HackPort`` repository at large. You *can*
   do this in one step by using

   ``git push --recurse-submodules=on-demand``

   but you can also do it manually.

   Otherwise, open a pull request for both ``gentoo-haskell/cabal`` and
   ``gentoo-haskell/hackport``.

Releasing a new version of HackPort
-----------------------------------

First, create a tag of your new ``HackPort`` version:

``git tag "v<version>" -s``

The ``-s`` option signs the tag with your GnuPG key, if you wish to do
this.

Next, run the ‘mk_release_tarball.bash’ script. This is similar to ``cabal sdist``
except that it also strips out unnecessary files and creates a tarball
from the latest tag rather than ``HEAD``.

Assuming everything builds correctly and you are a designated
``HackPort`` maintainer on Hackage, you can publish the ``HackPort``
source distribution:

``cabal upload --publish <tarball>``

Bump the ``HackPort`` ebuild in ``::haskell`` to the newest version
reflected on Hackage.

Further help and information
----------------------------

There will usually be a ``HackPort`` developer hanging about in
``#gentoo-haskell`` on FreeNode. Join the channel and ask away!

TODO
----

- Include section explaining how to determine and add the list of bundled
  libraries for a given GHC version to ``HackPort``.

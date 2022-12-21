
module Portage.Metadata.RemoteIdSpec where

import Control.Monad
import Test.Hspec hiding (Example(..))

import Portage.Metadata.RemoteId

spec :: Spec
spec = do
    describe "runUriParser" $ do
        it "parses URI as RemoteId correctly" $ do
            forM_ contrivedExamples $ \(Example u p r) ->
                runUriParser p u `shouldBe` Right r
        it "parses real-world examples correctly" $ do
            forM_ realWorldExamples $ \(Example u p r) ->
                runUriParser p u `shouldBe` Right r
    describe "matchURI" $ do
        it "matches URI correctly" $ do
            forM_ contrivedExamples $ \(Example u _ r) ->
                matchURI u `shouldBe` Just r
        it "matches real-world examples correctly" $ do
            forM_ realWorldExamples $ \(Example u _ r) ->
                matchURI u `shouldBe` Just r

data Example = Example
    { exampleURI :: String
    , exampleParser :: URIParser
    , exampleRemoteId :: RemoteId
    }

contrivedExamples :: [Example]
contrivedExamples =
    [ Example
        "https://cran.r-project.org/web/packages/foo/"
        cranParser
        (RemoteIdCRAN "foo")
    , Example
        "https://ctan.org/pkg/foo"
        ctanParser
        (RemoteIdCTAN "foo")
    , Example
        "https://gitweb.gentoo.org/foo.git/"
        gentooParser
        (RemoteIdGentoo "foo")
    , Example
        "https://github.com/foo/bar"
        githubParser
        (RemoteIdGithub "foo" "bar")
    , Example
        "https://gitlab.com/foo/bar"
        gitlabParser
        (RemoteIdGitlab "foo" "bar")
    , Example
        "https://launchpad.net/foo"
        launchpadParser
        (RemoteIdLaunchpad "foo")
    , Example
        "https://osdn.net/projects/foo/"
        osdnParser
        (RemoteIdOSDN "foo")
    , Example
        "https://pecl.php.net/package/foo"
        peclParser
        (RemoteIdPECL "foo")
    , Example
        "https://pypi.org/project/foo/"
        pypiParser
        (RemoteIdPyPI "foo")
    , Example
        "https://rubygems.org/gems/foo"
        rubygemsParser
        (RemoteIdRubygems "foo")
    , Example
        "https://sourceforge.net/projects/foo/"
        sourceforgeParser
        (RemoteIdSourceforge "foo")
    , Example
        "https://vim.org/scripts/script.php?script_id=foo"
        vimParser
        (RemoteIdVim "foo")
    ]

realWorldExamples :: [Example]
realWorldExamples =
    [ Example
        "git://github.com/gentoo-haskell/hackport.git"
        githubParser
        (RemoteIdGithub "gentoo-haskell" "hackport")
    , Example
        "https://github.com/haskell/parsec"
        githubParser
        (RemoteIdGithub "haskell" "parsec")
    , Example
        "https://github.com/dhall-lang/dhall-haskell/tree/master/dhall-toml"
        githubParser
        (RemoteIdGithub "dhall-lang" "dhall-haskell")
    , Example
        "https://github.com/gentoo-haskell/hackport#readme"
        githubParser
        (RemoteIdGithub "gentoo-haskell" "hackport")
    ]

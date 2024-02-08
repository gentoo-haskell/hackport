
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

-- | Since many parsers are nearly identical, and many of the hosts will never
--   source haskell code, these tests don't have to be exhaustive. If you run
--   into a URL that does not parse correctly, be sure to add it and the
--   expected result to 'realWorldExamples'.
contrivedExamples :: [Example]
contrivedExamples =
    [ Example
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
        "https://sourceforge.net/projects/foo/"
        sourceforgeParser
        (RemoteIdSourceforge "foo")
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
    , Example
        "https://codeberg.org/xmobar/xmobar"
        codebergParser
        (RemoteIdCodeberg "xmobar" "xmobar")
    , Example
        "git://codeberg.org/xmobar/xmobar.git"
        codebergParser
        (RemoteIdCodeberg "xmobar" "xmobar")
    , Example
        "https://invent.kde.org/plasma/sddm-kcm"
        kdeParser
        (RemoteIdKDE "plasma" "sddm-kcm")
    , Example
        "https://bitbucket.org/multicoreware/x265_git/"
        bitbucketParser
        (RemoteIdBitbucket "multicoreware" "x265_git")
    , Example
        "https://git.sr.ht/~steef/snixembed"
        sourcehutParser
        (RemoteIdSourcehut "~steef" "snixembed")
    , Example
        "https://sr.ht/~emersion/basu/"
        sourcehutParser
        (RemoteIdSourcehut "~emersion" "basu")
    ]

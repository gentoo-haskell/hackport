module Portage.PackageIdSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Data.Char as Char
import qualified Distribution.Package as Cabal
import           Distribution.Pretty (prettyShow)

import           Portage.PackageId
import           Portage.VersionSpec (ComplexVersion(..), AnySuffix(..))

spec :: Spec
spec = do
  describe "packageIdToFilePath" $ do
    prop "transforms a PackageId into a FilePath" $ do
      let cat = Category "dev-haskell"
          name = Cabal.mkPackageName "foo-bar2"
        in \(ComplexVersion version) ->
             packageIdToFilePath (PackageId (PackageName cat name) version)
             `shouldBe`
             "dev-haskell/" ++ prettyShow name ++ "/" ++ prettyShow name ++ "-" ++
             prettyShow version ++ ".ebuild"

  describe "filePathToPackageId" $ do
    -- The following property test fails if we randomly generate versionChar,
    -- so we leave it out.
    -- There is a bug in the Version parser. See 'parseFriendlyPackage' test
    -- below.
    prop "returns a Just PackageId from a valid FilePath" $ do
      \(NonNegative ver) (AnySuffix suf) (NonNegative r) ->
        let version = Version [ver] Nothing [suf] r
        in filePathToPackageId (Category "dev-haskell")
           ("foo-bar2-" ++ prettyShow version)
          `shouldBe`
           Just (PackageId (PackageName (Category "dev-haskell")
                            (Cabal.mkPackageName "foo-bar2"))
                 version)
    prop "returns Nothing on a malformed FilePath" $ do
      \(ComplexVersion version) ->
        filePathToPackageId (Category "dev-haskell") ("foo-bar-2-" ++ prettyShow version)
        `shouldBe`
        Nothing

  describe "normalizeCabalPackageName" $ do
    prop "converts a Cabal.PackageName into lowercase" $ do
      \(PrintableString s) -> normalizeCabalPackageName (Cabal.mkPackageName s)
                            `shouldBe` Cabal.mkPackageName (Char.toLower <$> s)

  describe "parseFriendlyPackage" $ do
    -- A bug in the Version parser causes this property test to fail
    -- if we randomly generate versionChar. Any value not equal to Nothing
    -- causes the underlying Parsec parser to fail with an error like:
    -- Left "\"<eitherParsec>\" (line 1, column 29):
    -- unexpected '_'
    -- expecting letter or digit, \"-\", white space or end of input"
    prop "parses a package string as [category/]name[-version]" $ do
      let cat = Category "dev-haskell"
          name = Cabal.mkPackageName "package-name1"
        in \(NonNegative ver) (AnySuffix suf) (NonNegative r) ->
             let version = Version [ver] Nothing [suf] r
             in parseFriendlyPackage (prettyShow cat ++ "/" ++ prettyShow name
                                      ++ "-" ++ prettyShow version)
             `shouldBe`
             Right (Just cat, name, Just version)
    prop "returns an error string if parsing a malformed package string" $ do
      let cat = Category "dev-haskell"
          name = Cabal.mkPackageName "package-name-1"
        in \(ComplexVersion version) ->
             parseFriendlyPackage (prettyShow cat ++ "/" ++ prettyShow name
                                   ++ "-" ++ prettyShow version)
             `shouldNotBe`
             Right (Just cat, name, Just version)

  describe "cabal_pn_to_PN" $ do
    prop "pretty-prints a Cabal PackageName as a lowercase String" $ do
      \(PrintableString s) -> cabal_pn_to_PN (Cabal.mkPackageName s)
                            `shouldBe` Char.toLower <$> s

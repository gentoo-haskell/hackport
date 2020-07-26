module Portage.PackageIdSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           QuickCheck.Instances

import qualified Data.Char as Char
import qualified Distribution.Package as Cabal
import           Distribution.Pretty (prettyShow)

import           Portage.PackageId

spec :: Spec
spec = do
  describe "packageIdToFilePath" $ do
    prop "transforms a PackageId into a FilePath" $ do
      let cat = Category "dev-haskell"
          name = Cabal.mkPackageName "foo-bar2+"
        in \(ComplexVersion version) ->
             packageIdToFilePath (PackageId (PackageName cat name) version)
             `shouldBe`
             "dev-haskell/" ++ prettyShow name ++ "/" ++ prettyShow name ++ "-" ++
             prettyShow version ++ ".ebuild"

  describe "filePathToPackageId" $ do
    prop "returns a Just PackageId from a valid FilePath" $ do
      let cat = Category "dev-haskell"
          name = Cabal.mkPackageName "foo-bar2+"
        in \(ComplexVersion version) ->
             filePathToPackageId cat
             (prettyShow name ++ "-" ++ prettyShow version)
             `shouldBe`
             Just (PackageId (PackageName cat name) version)
    prop "returns Nothing on a malformed FilePath" $ do
      let cat = Category "dev-haskell"
          name = Cabal.mkPackageName "foo-bar-2+"
        in \(ComplexVersion version) ->
             filePathToPackageId cat (prettyShow name ++ "-" ++ prettyShow version)
             `shouldBe`
             Nothing

  describe "normalizeCabalPackageName" $ do
    prop "converts a Cabal.PackageName into lowercase" $ do
      \(PrintableString s) -> normalizeCabalPackageName (Cabal.mkPackageName s)
                            `shouldBe` Cabal.mkPackageName (Char.toLower <$> s)

  describe "parseFriendlyPackage" $ do
    prop "parses a package string as [category/]name[-version]" $ do
      let cat = Category "dev-haskell"
          name = Cabal.mkPackageName "package-name1+"
        in \(ComplexVersion version) ->
             parseFriendlyPackage (prettyShow cat ++ "/" ++ prettyShow name
                                    ++ "-" ++ prettyShow version)
             `shouldBe`
             Right (Just cat, name, Just version)
    prop "returns an error string if parsing a malformed package string" $ do
      let cat = Category "dev-haskell"
          name = Cabal.mkPackageName "package-name-1+"
        in \(ComplexVersion version) ->
             parseFriendlyPackage (prettyShow cat ++ "/" ++ prettyShow name
                                   ++ "-" ++ prettyShow version)
             `shouldNotBe`
             Right (Just cat, name, Just version)

  describe "cabal_pn_to_PN" $ do
    prop "pretty-prints a Cabal PackageName as a lowercase String" $ do
      \(PrintableString s) -> cabal_pn_to_PN (Cabal.mkPackageName s)
                            `shouldBe` Char.toLower <$> s

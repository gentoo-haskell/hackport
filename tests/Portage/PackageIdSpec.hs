module Portage.PackageIdSpec (spec) where

import Test.Hspec

import qualified Distribution.Package as Cabal

import Portage.PackageId
import qualified Portage.Version as Version

spec :: Spec
spec = do
  describe "packageIdToFilePath" $ do
    it "transforms a PackageId into a FilePath" $ do
      packageIdToFilePath (PackageId (PackageName (Category "dev-haskell")
                                      (Cabal.mkPackageName "foo-bar2"))
                            (Version.Version [3,0,0] (Just 'b')
                              [Version.RC 2] 1 ))
        `shouldBe`
        "dev-haskell/foo-bar2/foo-bar2-3.0.0b_rc2-r1.ebuild"
  describe "filePathToPackageId" $ do
    it "generates a Just PackageId from a FilePath" $ do
      filePathToPackageId (Category "dev-haskell") "foo-bar2-3.0.0b_rc2-r1"
        `shouldBe`
        Just (PackageId (PackageName (Category "dev-haskell")
                          (Cabal.mkPackageName "foo-bar2"))
               (Version.Version [3,0,0] (Just 'b') [Version.RC 2] 1))
    it "returns Nothing on a malformed FilePath" $ do
      filePathToPackageId (Category "dev-haskell") "foo-bar-2-3.0.0b_rc2-r1"
        `shouldBe`
        Nothing
  describe "normalizeCabalPackageName" $ do
    it "converts a Cabal.PackageName into lowercase" $ do
      normalizeCabalPackageName (Cabal.mkPackageName "FooBar1")
      `shouldBe` Cabal.mkPackageName "foobar1"
  describe "parseFriendlyPackage" $ do
    it "parses a package string as [category/]name[-version]" $ do
      parseFriendlyPackage "category-name/package-name1-0.0.0.1a_beta2-r4"
        `shouldBe`
        Right (Just (Category "category-name"),
                Cabal.mkPackageName "package-name1",
                Just (Version.Version [0,0,0,1] (Just 'a') [Version.Beta 2] 4))
    it "returns an error string if parsing a malformed package string" $ do
      parseFriendlyPackage "category-name/package-name-1-0.0.0.1a_beta2-r4"
        `shouldBe`
        Left "\"<eitherParsec>\" (line 1, column 29):\nunexpected \"0\"\nexpecting \"-r\""
  describe "cabal_pn_to_PN" $ do
    it "pretty-prints a Cabal PackageName as a lowercase String" $ do
      cabal_pn_to_PN (Cabal.mkPackageName "FooBar1") `shouldBe` "foobar1"

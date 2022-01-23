module Portage.GHCCoreSpec (spec) where

import Test.Hspec

import qualified Distribution.Version             as Cabal
import qualified Distribution.Package             as Cabal

import Portage.GHCCore

spec :: Spec
spec = do
  describe "cabalFromGHC" $ do
    it "returns the corresponding Cabal version for a valid GHC version" $ do
      cabalFromGHC [8,8,3] `shouldBe` Just (Cabal.mkVersion [3,0,1,0])
    it "returns Nothing for an invalid GHC version" $ do
      cabalFromGHC [9,9,9,9] `shouldBe` Nothing
  describe "packageIsCoreInAnyGHC" $ do
    it "returns True for the binary package" $ do
      packageIsCoreInAnyGHC (Cabal.mkPackageName "binary") `shouldBe` True
    it "returns False for the haskeline package (because it is upgradeable)" $ do
      packageIsCoreInAnyGHC (Cabal.mkPackageName "haskeline") `shouldBe` False
    it "returns False for the yesod package" $ do
      packageIsCoreInAnyGHC (Cabal.mkPackageName "yesod") `shouldBe` False

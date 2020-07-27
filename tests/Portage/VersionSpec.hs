module Portage.VersionSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           QuickCheck.Instances

import qualified Distribution.Version as Cabal

import           Portage.Version

spec :: Spec
spec = do
  describe "is_live" $ do
    prop "determines if a Portage version is live" $ do
      \(ComplexVersion v) -> is_live v `shouldBe`
        if last (versionNumber v) >= 9999
        then True else False
        
  describe "fromCabalVersion" $ do
    prop "converts from a Cabal version to a Portage version" $ do
      \verNum -> fromCabalVersion (Cabal.mkVersion verNum) == Version verNum Nothing [] 0
      
  describe "toCabalVersion" $ do
    prop "converts from a Portage version to a Cabal version" $ do
      \(ComplexVersion v) -> toCabalVersion v `shouldBe`
        if versionChar v == Nothing && versionSuffix v == []
        then Just (Cabal.mkVersion (versionNumber v)) else Nothing

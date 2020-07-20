module Portage.VersionSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Distribution.Version as Cabal

import Portage.Version

spec :: Spec
spec = do
  describe "is_live" $ do
    prop "determines if a Portage version is live" $ do
      \verNum char rev -> is_live (Version verNum char [] rev) ==
        if length verNum >= 1 && last verNum >= 9999 then True else False
  describe "fromCabalVersion" $ do
    prop "converts from a Cabal version to a Portage version" $ do
      \verNum -> fromCabalVersion (Cabal.mkVersion verNum) == Version verNum Nothing [] 0
  describe "toCabalVersion" $ do
    prop "converts from a Portage version to a Cabal version" $ do
      \verNum char rev -> toCabalVersion (Version verNum char [] rev) ==
        if char == Nothing then Just (Cabal.mkVersion verNum) else Nothing

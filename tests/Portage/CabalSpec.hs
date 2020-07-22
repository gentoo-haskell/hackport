module Portage.CabalSpec (spec) where

import Test.Hspec

import qualified Distribution.SPDX as SPDX

import Portage.Cabal

spec :: Spec
spec = do
  describe "convertLicense" $ do
    it "converts a license into a Gentoo-style license string" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.GPL_3_0_or_later)
        `shouldBe`
        Right "GPL-3"
    it "converts an unknown license into an error string" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.EUPL_1_1)
        `shouldBe`
        Left "license unknown to cabal. Please pick it manually."

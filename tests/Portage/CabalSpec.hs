module Portage.CabalSpec (spec) where

import Test.Hspec

import qualified Distribution.SPDX as SPDX

import Portage.Cabal

spec :: Spec
spec = do
  describe "convertLicense" $ do
    it "converts AGPL_3_0_or_later into AGPL-3+" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.AGPL_3_0_or_later)
        `shouldBe`
        Right "AGPL-3+"
    it "converts AGPL_3_0_only into AGPL-3" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.AGPL_3_0_only)
        `shouldBe`
        Right "AGPL-3"
    it "converts GPL_2_0_or_later into GPL-2+" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.GPL_2_0_or_later)
        `shouldBe`
        Right "GPL-2+"
    it "converts GPL_2_0_only into GPL-2" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.GPL_2_0_only)
        `shouldBe`
        Right "GPL-2"
    it "converts GPL_3_0_or_later into GPL-3+" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.GPL_3_0_or_later)
        `shouldBe`
        Right "GPL-3+"
    it "converts GPL_3_0_only into GPL-3" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.GPL_3_0_only)
        `shouldBe`
        Right "GPL-3"
    -- Unfortunately, Cabal treats LGPL_2_0_only and LGPL_2_0_or_later as identical,
    -- just as it does with LGPL_2_1_only and LGPL_2_1_or_later. This means
    -- we cannot handle LGPL-2.0+ or LGPL-2,1+ without directly dealing with
    -- the SPDX licence, before it is converted into a Cabal-style licence.
    it "converts LGPL_2_0_or_later into LGPL-2, without a trailing plus (+)" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.LGPL_2_0_or_later)
        `shouldBe`
        Right "LGPL-2"
    it "converts LGPL_2_0_only into LGPL-2" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.LGPL_2_0_only)
        `shouldBe`
        Right "LGPL-2"
    it "converts LGPL_2_1_or_later into LGPL-2.1, without a trailing plus (+)" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.LGPL_2_1_or_later)
        `shouldBe`
        Right "LGPL-2.1"
    it "converts LGPL_2_1_only into LGPL-2.1" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.LGPL_2_1_only)
        `shouldBe`
        Right "LGPL-2.1"
      -- But these next two cases are correctly handled:
    it "converts LGPL_3_0_or_later into LGPL-3+" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.LGPL_3_0_or_later)
        `shouldBe`
        Right "LGPL-3+"
    it "converts LGPL_3_0_only into LGPL-3" $ do
      convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.LGPL_3_0_only)
        `shouldBe`
        Right "LGPL-3"
    context "when a licence string is invalid" $ do
      it "converts the unknown licence into an error string" $ do
        convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.EUPL_1_1)
          `shouldBe`
          Left "license unknown to cabal. Please pick it manually."

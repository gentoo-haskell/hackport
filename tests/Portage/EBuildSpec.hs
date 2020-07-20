module Portage.EBuildSpec (spec) where

import Test.Hspec

import Portage.EBuild

spec :: Spec
spec = do
  describe "src_uri" $ do
    it "returns the default SRC_URI for a lowercase package name" $ do
      src_uri ebuildTemplate
        `shouldBe` "https://hackage.haskell.org/package/${P}/${P}.tar.gz"
    it "returns the case-sensitive SRC_URI for a case-sensitive package name" $ do
      src_uri ebuildTemplate { my_pn = Just "Foo_Bar" }
        `shouldBe` "https://hackage.haskell.org/package/${MY_P}/${MY_P}.tar.gz"
  describe "sort_iuse" $ do
    it "sorts IUSE alphabetically despite pluses (+)" $ do
      sort_iuse ["+a","c","+b","d"] `shouldBe` ["+a","+b","c","d"]
  describe "drop_tdot" $ do
    it "drops trailing dots (.)" $ do
      drop_tdot "foo." `shouldBe` "foo"
      drop_tdot "foo..." `shouldBe` "foo"
      drop_tdot "foo" `shouldBe` "foo"
  describe "quote" $ do
    it "should correctly surround a string with special characters in quotes" $ do
      quote "Reading, writing and manipulating '.tar' archives." ""
        `shouldBe`
        "\"Reading, writing and manipulating \'.tar\' archives.\""
      

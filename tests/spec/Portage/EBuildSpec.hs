module Portage.EBuildSpec (spec) where

import Test.Hspec

import Portage.EBuild

spec :: Spec
spec = do
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
      quote "Extras for the \"contravariant\" package" ""
        `shouldBe`
        "\"Extras for the \\\"contravariant\\\" package\""
  describe "toHttps" $ do
    it "should not convert whitelisted http-only homepages into https homepages" $ do
      toHttps "http://leksah.org" `shouldBe` "http://leksah.org"
      toHttps "http://darcs.net/" `shouldBe` "http://darcs.net/"
    it "should otherwise convert all homepages into https-aware homepages" $ do
      toHttps "http://pandoc.org" `shouldBe` "https://pandoc.org"
      toHttps "http://www.yesodweb.com/" `shouldBe` "https://www.yesodweb.com/"
    it "should ignore any conversions of homepages already marked as https-aware" $ do
      toHttps "https://github.com" `shouldBe` "https://github.com"

module Portage.MetadataSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.List       as L
import qualified Data.Text       as T
import qualified Data.Map.Strict as Map

import Portage.Metadata

spec :: Spec
spec = do
  describe "pureMetadataFromFile" $ do
    it "returns Nothing from an empty Text" $ do
      pureMetadataFromFile T.empty `shouldBe` Nothing
    it "equals makeMinimalMetadata with no USE flags" $ do
      pureMetadataFromFile (makeDefaultMetadata Map.empty) `shouldBe` Just makeMinimalMetadata
    it "equals makeMinimalMetadata plus the supplied USE flags" $ do
      let flags = Map.singleton "name" "description"
      pureMetadataFromFile (makeDefaultMetadata flags) `shouldBe` Just (makeMinimalMetadata { metadataUseFlags = flags })

  describe "stripGlobalUseFlags" $ do
    it "should remove specified global USE flags from the metadata.xml" $ do
      stripGlobalUseFlags (Map.singleton "debug" "description") `shouldBe` Map.empty
      stripGlobalUseFlags (Map.singleton "examples" "description") `shouldBe` Map.empty
      stripGlobalUseFlags (Map.singleton "static" "description") `shouldBe` Map.empty
    prop "should ignore USE flags that are not specified as global" $ do
      \name description -> stripGlobalUseFlags (Map.singleton name description) ==
                           if name `elem` ["debug","examples","static"]
                           then Map.empty
                           else Map.singleton name description

  describe "prettyPrintFlags" $ do
    it "correctly handles special XML characters contained in strings" $ do
      let name = "foo"
          desc = "bar < 1.1.0"
        in prettyPrintFlags (Map.singleton name desc) `shouldBe`
        ["\t\t<flag name=\"" ++ name ++ "\">"
         ++ "bar &lt; 1.1.0" ++ "</flag>"]
    it "correctly formats a single USE flag name with its description" $ do
      let name = "foo"
          description = "bar"
        in prettyPrintFlags (Map.singleton name description) `shouldBe`
                           ["\t\t<flag name=\"" ++ name ++
                           "\">" ++ (L.intercalate " " . lines $ description)
                           ++ "</flag>"]
    it "correctly formats multiple USE flag names with their descriptions" $ do
      let f1 = "flag1"
          f2 = "flag2"
          d1 = "foo_desc"
          d2 = "bar_desc"
        in prettyPrintFlags (Map.fromList [(f1,d1),(f2,d2)]) `shouldBe`
           ["\t\t<flag name=\"" ++ f1
           ++ "\">" ++ (L.intercalate " " . lines $ d1)
           ++ "</flag>"
           ,
           "\t\t<flag name=\"" ++ f2
           ++ "\">" ++ (L.intercalate " " . lines $ d2)
           ++ "</flag>"]

    prop "should have a length equal to the number of USE flags" $ do
      \flags -> length (prettyPrintFlags flags) == Map.size flags
      
  describe "makeDefaultMetadata" $ do
    context "when writing a minimal metadata.xml with no USE flags" $ do
      it "should have a certain number of lines" $ do
        -- This is the number of lines in a skeleton metadata.xml.
        -- If it does not equal this number, the formatting may be wrong.
        length (T.lines (makeDefaultMetadata Map.empty)) `shouldBe` 8
      it "should have a certain format" $ do
        let correctMetadata = T.pack $ unlines
              [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
              , "<!DOCTYPE pkgmetadata SYSTEM \"http://www.gentoo.org/dtd/metadata.dtd\">"
              , "<pkgmetadata>"
              , "\t<maintainer type=\"project\">"
              , "\t\t<email>haskell@gentoo.org</email>"
              , "\t\t<name>Gentoo Haskell</name>"
              , "\t</maintainer>"
              , "</pkgmetadata>"
              ]
          in makeDefaultMetadata Map.empty `shouldBe` correctMetadata
    context "when writing a metadata.xml with USE flags" $ do
      it "should have a certain number of lines" $ do
        let flags = Map.singleton "name" "description"
          in length (T.lines (makeDefaultMetadata flags))
             `shouldBe` 10 + (Map.size flags)
      it "should have a certain format, including the <use> element" $ do
        let flags = Map.fromList [("flag1","desc1"),("flag2","desc2")]
            correctMetadata = T.pack $ unlines
              [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
              , "<!DOCTYPE pkgmetadata SYSTEM \"http://www.gentoo.org/dtd/metadata.dtd\">"
              , "<pkgmetadata>"
              , "\t<maintainer type=\"project\">"
              , "\t\t<email>haskell@gentoo.org</email>"
              , "\t\t<name>Gentoo Haskell</name>"
              , "\t</maintainer>"
              , "\t<use>"
              , "\t\t<flag name=\"flag1\">desc1</flag>"
              , "\t\t<flag name=\"flag2\">desc2</flag>"
              , "\t</use>"
              , "</pkgmetadata>"
              ]
          in makeDefaultMetadata flags `shouldBe` correctMetadata

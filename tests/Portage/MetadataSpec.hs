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

  describe "prettyPrintFlags" $ do
    prop "should correctly format a single USE flag name with its description" $ do
      \name description -> prettyPrintFlags (Map.singleton name description) ==
                           ["\t\t<flag name=\"" ++ name ++
                           "\">" ++ (L.intercalate " " . lines $ description)
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
        let flags = Map.singleton "name" "description"
            correctMetadata = T.pack $ unlines
              [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
              , "<!DOCTYPE pkgmetadata SYSTEM \"http://www.gentoo.org/dtd/metadata.dtd\">"
              , "<pkgmetadata>"
              , "\t<maintainer type=\"project\">"
              , "\t\t<email>haskell@gentoo.org</email>"
              , "\t\t<name>Gentoo Haskell</name>"
              , "\t</maintainer>"
              , "\t<use>"
              , "\t\t<flag name=\"name\">description</flag>"
              , "\t</use>"
              , "</pkgmetadata>"
              ]
          in makeDefaultMetadata flags `shouldBe` correctMetadata

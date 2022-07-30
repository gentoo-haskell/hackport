module Portage.MetadataSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.Text       as T
import qualified Data.Map.Strict as Map
import qualified Data.Set        as S

import qualified Portage.EBuild as E
import Portage.Metadata
import Portage.Metadata.RemoteId

spec :: Spec
spec = do
-- TODO: These tests are based off old behavior and should be remade at some point
--   describe "parseMetadataXML" $ do
--     it "returns Nothing from an empty Text" $ do
--       parseMetadataXML T.empty `shouldBe` Nothing
--     it "equals makeMinimalMetadata with no USE flags" $ do
--       parseMetadataXML (printMetadata Map.empty) `shouldBe` Just minimalMetadata
--     it "equals makeMinimalMetadata plus the supplied USE flags" $ do
--       let flags = Map.singleton "name" "description"
--       parseMetadataXML (makeDefaultMetadata flags) `shouldBe` Just (makeMinimalMetadata { metadataUseFlags = flags })

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
        [ "\t<use>"
        , "\t\t<flag name=\"" ++ name ++ "\">"
            ++ "bar &lt; 1.1.0" ++ "</flag>"
        , "\t</use>"
        ]
    it "correctly formats a single USE flag name with its description" $ do
      let name = "foo"
          description = "bar"
        in prettyPrintFlags (Map.singleton name description) `shouldBe`
                          [ "\t<use>"
                          , "\t\t<flag name=\"" ++ name
                              ++ "\">" ++ (unwords . lines $ description)
                              ++ "</flag>"
                          , "\t</use>"
                          ]
    it "correctly formats multiple USE flag names with their descriptions" $ do
      let f1 = "flag1"
          f2 = "flag2"
          d1 = "foo_desc"
          d2 = "bar_desc"
        in prettyPrintFlags (Map.fromList [(f1,d1),(f2,d2)]) `shouldBe`
          [ "\t<use>"
          , "\t\t<flag name=\"" ++ f1
              ++ "\">" ++ (unwords . lines $ d1)
              ++ "</flag>"
          , "\t\t<flag name=\"" ++ f2
              ++ "\">" ++ (unwords . lines $ d2)
              ++ "</flag>"
          , "\t</use>"
          ]

  describe "printMetadata" $ do
    context "when writing a minimal metadata.xml with no USE flags" $ do
      it "should have a certain format" $
        let correctMetadata = T.pack $ unlines
              [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
              , "<!DOCTYPE pkgmetadata SYSTEM \"https://www.gentoo.org/dtd/metadata.dtd\">"
              , "<pkgmetadata>"
              , "\t<maintainer type=\"project\">"
              , "\t\t<email>haskell@gentoo.org</email>"
              , "\t\t<name>Gentoo Haskell</name>"
              , "\t</maintainer>"
              , "\t<upstream>"
              , "\t\t<remote-id type=\"hackage\">FooBar</remote-id>"
              , "\t</upstream>"
              , "</pkgmetadata>"
              ]
        in printMetadata (minimalMetadata True E.ebuildTemplate) `shouldBe` correctMetadata
    context "when writing a minimal metadata.xml with no USE flags and --no-hackage-remote-id" $ do
      it "should have a certain format" $
        let correctMetadata = T.pack $ unlines
              [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
              , "<!DOCTYPE pkgmetadata SYSTEM \"https://www.gentoo.org/dtd/metadata.dtd\">"
              , "<pkgmetadata>"
              , "\t<maintainer type=\"project\">"
              , "\t\t<email>haskell@gentoo.org</email>"
              , "\t\t<name>Gentoo Haskell</name>"
              , "\t</maintainer>"
              , "</pkgmetadata>"
              ]
        in printMetadata (minimalMetadata False E.ebuildTemplate) `shouldBe` correctMetadata
    context "when writing a metadata.xml with USE flags and a GitHub remote-id" $ do
      it "should have a certain format, including the <use> element" $ do
        let meta = minimalMetadata True E.ebuildTemplate <> mempty
              { metadataUseFlags = Map.fromList [("flag1","desc1"),("flag2","desc2")]
              , metadataRemoteIds = S.singleton $ RemoteIdGithub "foo" "bar"
              }
            correctMetadata = T.pack $ unlines
              [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
              , "<!DOCTYPE pkgmetadata SYSTEM \"https://www.gentoo.org/dtd/metadata.dtd\">"
              , "<pkgmetadata>"
              , "\t<maintainer type=\"project\">"
              , "\t\t<email>haskell@gentoo.org</email>"
              , "\t\t<name>Gentoo Haskell</name>"
              , "\t</maintainer>"
              , "\t<use>"
              , "\t\t<flag name=\"flag1\">desc1</flag>"
              , "\t\t<flag name=\"flag2\">desc2</flag>"
              , "\t</use>"
              , "\t<upstream>"
              , "\t\t<remote-id type=\"hackage\">FooBar</remote-id>"
              , "\t\t<remote-id type=\"github\">foo/bar</remote-id>"
              , "\t</upstream>"
              , "</pkgmetadata>"
              ]
          in printMetadata meta `shouldBe` correctMetadata

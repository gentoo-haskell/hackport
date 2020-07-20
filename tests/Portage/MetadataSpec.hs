module Portage.MetadataSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.Text as T
import qualified Data.Map.Strict as Map
import Portage.Metadata

spec :: Spec
spec = do
  describe "pureMetadataFromFile" $ do
    it "always returns Nothing from an empty Text" $ do
      pureMetadataFromFile T.empty `shouldBe` Nothing
    prop ("always equals makeMinimalMetadata with no USE flags," ++
          " no matter the description") $ do
      \desc -> pureMetadataFromFile (T.pack (makeDefaultMetadata desc Map.empty)) ==
               Just makeMinimalMetadata
    prop ("always equals makeMinimalMetadata plus the supplied USE flags," ++
          " no matter the description") $ do
      let flags = Map.fromList [("name","description")]
        in \desc -> pureMetadataFromFile (T.pack (makeDefaultMetadata desc flags))
                    == Just (makeMinimalMetadata { metadataUseFlags = flags } )

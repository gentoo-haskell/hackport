module Portage.Metadata
        ( Metadata(..)
        , metadataFromFile
        ) where

import qualified Data.ByteString as B

import Control.Applicative

import Text.XML.Light

import Control.Monad

data Metadata = Metadata
      { metadataHerds :: [String]
      -- , metadataMaintainers :: [String],
      -- , metadataUseFlags :: [(String,String)]
      } deriving (Show)

metadataFromFile :: FilePath -> IO (Maybe Metadata)
metadataFromFile fp = do
  doc <- parseXMLDoc <$> B.readFile fp
  return (doc >>= parseMetadata)

parseMetadata :: Element -> Maybe Metadata
parseMetadata xml = do
  let herds = map strContent (findChildren (unqual "herd") xml)
  return Metadata
            {
              metadataHerds = herds
            }

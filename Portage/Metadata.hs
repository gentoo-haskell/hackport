module Portage.Metadata
        ( Metadata(..)
        , fromFile
        ) where

import qualified Data.ByteString as B

import Control.Applicative

import Text.XML.Light

import Debug.Trace ( trace )
import System
import Control.Monad

data Metadata = Metadata
      { metadataHerds :: [String]
      -- , metadataMaintainers :: [String],
      -- , metadataUseFlags :: [(String,String)]
      } deriving (Show)

fromFile :: FilePath -> IO (Maybe Metadata)
fromFile fp = do
  doc <- parseXMLDoc <$> B.readFile fp
  return (doc >>= parseMetadata)

parseMetadata :: Element -> Maybe Metadata
parseMetadata xml = do
  let herds = map strContent (findChildren (unqual "herd") xml)
  return Metadata
            {
              metadataHerds = herds
            }

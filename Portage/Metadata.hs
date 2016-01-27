module Portage.Metadata
        ( Metadata(..)
        , metadataFromFile
        , makeDefaultMetadata
        ) where

import qualified Data.ByteString as B

import Control.Applicative

import Text.XML.Light

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

-- don't use Text.XML.Light as we like our own pretty printer
makeDefaultMetadata :: String -> String
makeDefaultMetadata long_description =
    unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<!DOCTYPE pkgmetadata SYSTEM \"http://www.gentoo.org/dtd/metadata.dtd\">"
            , "<pkgmetadata>"
            , "\t<maintainer type=\"project\">"
            , "\t\t<email>haskell@gentoo.org</email>"
            , "\t\t<name>Gentoo Haskell</name>"
            , "\t</maintainer>"
            , (init {- strip trailing newline-}
              . unlines
              . map (\l -> if l `elem` ["<longdescription>", "</longdescription>"]
                               then "\t"   ++ l -- leading/trailing lines
                               else "\t\t" ++ l -- description itself
                    )
              . lines
              . showElement
              . unode "longdescription"
              . ("\n" ++) -- prepend newline to separate form <longdescription>
              . (++ "\n") -- append newline
              ) long_description
            , "</pkgmetadata>"
            ]

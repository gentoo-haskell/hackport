module Portage.Metadata
        ( Metadata(..)
        , metadataFromFile
        , makeDefaultMetadata
        ) where

import qualified Data.ByteString as B

import Control.Applicative

import Text.XML.Light

data Metadata = Metadata
      { metadata_emails :: [String]
      -- , metadataMaintainers :: [String],
      -- , metadataUseFlags :: [(String,String)]
      } deriving (Show)

metadataFromFile :: FilePath -> IO (Maybe Metadata)
metadataFromFile fp = do
  doc <- parseXMLDoc <$> B.readFile fp
  return (doc >>= parseMetadata)

parseMetadata :: Element -> Maybe Metadata
parseMetadata xml =
  return Metadata { metadata_emails = map strContent (findElements (unqual "email") xml) }

formatFlags :: (String, String) -> String
formatFlags (name, description) = "\t\t<flag name=\"" ++ name ++
                                  "\">" ++ description ++ "</flag>"

-- don't use Text.XML.Light as we like our own pretty printer
makeDefaultMetadata :: String -> [(String, String)] -> String
makeDefaultMetadata long_description flags =
    unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<!DOCTYPE pkgmetadata SYSTEM \"http://www.gentoo.org/dtd/metadata.dtd\">"
            , "<pkgmetadata>"
            , "\t<maintainer type=\"project\">"
            , "\t\t<email>haskell@gentoo.org</email>"
            , "\t\t<name>Gentoo Haskell</name>"
            , "\t</maintainer>"
            , if (formatFlags <$> flags) == [""]
              then "\t<use>\n\t</use>"
              else "\t<use>\n" ++ (unlines $ formatFlags <$> flags) ++
                   "\t</use>"
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

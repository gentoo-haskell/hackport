module Portage.Metadata
        ( Metadata(..)
        , metadataFromFile
        , pureMetadataFromFile
        , prettyPrintFlagsHuman
        , makeDefaultMetadata
        , makeMinimalMetadata
        ) where

import qualified AnsiColor as A

import qualified Data.Text as T
import qualified Data.Text.IO as T
import qualified Data.Map.Strict as Map

import Text.XML.Light

data Metadata = Metadata
      { metadataEmails :: [String]
      , metadataUseFlags :: Map.Map String String
      -- , metadataMaintainers :: [String]
      } deriving (Show)

-- | Maybe return a Metadata from a Text string
pureMetadataFromFile :: T.Text -> Maybe Metadata
pureMetadataFromFile file = parseXMLDoc file >>= \doc -> parseMetadata doc

-- | Apply @pureMetadataFromFile@ to a FilePath
metadataFromFile :: FilePath -> IO (Maybe Metadata)
metadataFromFile fp = pureMetadataFromFile <$> T.readFile fp

-- | Extract the maintainer email and USE flags from a supplied XML Element
parseMetadata :: Element -> Maybe Metadata
parseMetadata xml =
  return Metadata { metadataEmails = strContent <$> findElements (unqual "email") xml
                  , metadataUseFlags =
                      -- find the flag name
                      let x = findElement (unqual "use") xml
                          y = onlyElems $ concatMap elContent x
                          z = attrVal <$> concatMap elAttribs y
                      -- find the flag description
                          a = concatMap elContent y
                          b = cdData <$> onlyText a
                        in Map.fromList $ zip z b
                  }

-- | Pretty print as valid XML a list of flags and their descriptions
-- from a given strict map.
prettyPrintFlags :: Map.Map String String -> [String]
prettyPrintFlags m = (\(name,description) -> "\t\t<flag name=\"" ++ name ++
                                          "\">" ++ description ++ "</flag>")
                     <$> Map.toAscList m

-- | Pretty print a human-readable list of flags and their descriptions
-- from a given strict map.
prettyPrintFlagsHuman :: Map.Map String String -> [String]
prettyPrintFlagsHuman m = (\(name,description) -> A.bold (name ++ ": ") ++ description)
                          <$> Map.toAscList m
                          
-- | A minimal metadata for use as a fallback value
makeMinimalMetadata :: Metadata
makeMinimalMetadata = Metadata { metadataEmails = ["haskell@gentoo.org"]
                               , metadataUseFlags = Map.empty
                               }

-- don't use Text.XML.Light as we like our own pretty printer
-- | Pretty print the metadata.xml file
makeDefaultMetadata :: String -> Map.Map String String -> String
makeDefaultMetadata long_description flags =
    unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
            , "<!DOCTYPE pkgmetadata SYSTEM \"http://www.gentoo.org/dtd/metadata.dtd\">"
            , "<pkgmetadata>"
            , "\t<maintainer type=\"project\">"
            , "\t\t<email>haskell@gentoo.org</email>"
            , "\t\t<name>Gentoo Haskell</name>"
            , "\t</maintainer>"
            , "\t<use>\n" ++ (unlines $ prettyPrintFlags flags) ++ "\t</use>"
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

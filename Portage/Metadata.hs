{-|
Module      : Portage.Metadata
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Functions and types related to @metadata.xml@ processing
-}
module Portage.Metadata
        ( Metadata(..)
        , metadataFromFile
        , pureMetadataFromFile
        , prettyPrintFlags -- exported for hspec
        , prettyPrintFlagsHuman
        , makeDefaultMetadata
        , makeMinimalMetadata
        ) where

import qualified AnsiColor as A

import qualified Data.List       as L
import qualified Data.Map.Strict as Map
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import Text.XML.Light

-- | A data type for the Gentoo-specific @metadata.xml@ file.
-- Currently defines functions for the maintainer email and
-- USE flags and their descriptions.
data Metadata = Metadata
      { metadataEmails :: [String] -- ^ This should /always/ be [\"haskell@gentoo.org\"].
      , metadataUseFlags :: Map.Map String String -- ^ Only /active/ USE flags, if any.
      } deriving (Eq, Show)

-- | Maybe return a 'Metadata' from a 'T.Text'.
--
-- Trying to parse an empty 'T.Text' should return 'Nothing':
--
-- prop> pureMetadataFromFile T.empty == Nothing
--
-- Parsing a @metadata.xml@ /without/ USE flags should /always/ be equivalent
-- to 'makeMinimalMetadata', no matter the package description:
--
-- prop> \desc -> pureMetadataFromFile (makeDefaultMetadata desc Map.empty) == Just makeMinimalMetadata
--
-- Parsing a @metadata.xml@ /with/ USE flags should /always/ be equivalent
-- to 'makeMinimalMetadata' /plus/ the supplied USE flags, no matter the
-- package description:
--
-- prop> \desc -> pureMetadataFromFile (makeDefaultMetadata desc (Map.fromList [("name","description")])) == Just (makeMinimalMetadata {metadataUseFlags = Map.fromList [("name","description")] } )
pureMetadataFromFile :: T.Text -> Maybe Metadata
pureMetadataFromFile file = parseXMLDoc file >>= \doc -> parseMetadata doc

-- | Apply 'pureMetadataFromFile' to a 'FilePath'.
metadataFromFile :: FilePath -> IO (Maybe Metadata)
metadataFromFile fp = pureMetadataFromFile <$> T.readFile fp

-- | Extract the maintainer email and USE flags from a supplied XML 'Element'.
-- 
-- If we're parsing a blank 'Element' or otherwise empty @metadata.xml@:
-- >>> parseMetadata blank_element
-- Just (Metadata {metadataEmails = [], metadataUseFlags = fromList []})
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
-- from a given 'Map.Map'.
prettyPrintFlags :: Map.Map String String -> [String]
prettyPrintFlags m = (\(name,description) ->
                        "\t\t<flag name=\"" ++ name ++ "\">" ++
                        (L.intercalate " " . lines $ description) ++ "</flag>")
                     <$> Map.toAscList m

-- | Pretty print a human-readable list of flags and their descriptions
-- from a given 'Map.Map'.
prettyPrintFlagsHuman :: Map.Map String String -> [String]
prettyPrintFlagsHuman m = (\(name,description) -> A.bold (name ++ ": ") ++
                            (L.intercalate " " . lines $ description))
                          <$> Map.toAscList m
                          
-- | A minimal metadata for use as a fallback value.
makeMinimalMetadata :: Metadata
makeMinimalMetadata = Metadata { metadataEmails = ["haskell@gentoo.org"]
                               , metadataUseFlags = Map.empty
                               }

-- don't use Text.XML.Light as we like our own pretty printer
-- | Pretty print the @metadata.xml@ string.
--
-- This function will additionally print the @<use>@ and @<longdescription>@
-- xml elements if:
--
--   1. There are USE flags to fill the @<use>@ element, which cannot be empty;
--      and
--   2. The long description is greater than 150 characters long.
--
-- The long description also cannot be identical to the @DESCRIPTION@ field
-- in an ebuild in order to pass Gentoo QA, but this is checked for outside
-- of this function. See "Merge" for an example usage.
makeDefaultMetadata :: String -> Map.Map String String -> T.Text
makeDefaultMetadata long_description flags = T.pack $
  unlines [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
          , "<!DOCTYPE pkgmetadata SYSTEM \"http://www.gentoo.org/dtd/metadata.dtd\">"
          , "<pkgmetadata>"
          , "\t<maintainer type=\"project\">"
          , "\t\t<email>haskell@gentoo.org</email>"
          , "\t\t<name>Gentoo Haskell</name>"
          , "\t</maintainer>"
            ++ if flags == Map.empty
               then []
               else "\n\t<use>\n" ++ (unlines $ prettyPrintFlags flags) ++ "\t</use>"
            ++ if long_description == "" || length long_description <= 150
               then []
               else "\n" ++
                    (init {- strip trailing newline-}
                      . unlines
                      . map (\l -> if l `elem` ["<longdescription>", "</longdescription>"]
                                   then "\t"   ++ l -- leading/trailing lines
                                   else "\t\t" ++ l -- description itself
                            )
                      . lines
                      . showElement
                      . unode "longdescription"
                      . ("\n" ++) -- prepend newline to separate from <longdescription>
                      . (++ "\n") -- append newline
                    ) long_description
          , "</pkgmetadata>"
          ]

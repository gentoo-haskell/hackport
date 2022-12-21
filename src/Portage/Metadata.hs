{-|
Module      : Portage.Metadata
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Functions and types related to @metadata.xml@ processing
-}
module Portage.Metadata
        ( Metadata(..)
        , UseFlags
        , updateMetadata
        , readMetadataFile
        , parseMetadataXML
        , stripGlobalUseFlags -- exported for hspec
        , prettyPrintFlags -- exported for hspec
        , prettyPrintFlagsHuman
        , printMetadata
        , minimalMetadata
        ) where

import qualified AnsiColor as A

import Data.Maybe (fromMaybe)
import qualified Data.Map.Strict as Map
import qualified Data.Set        as S
import qualified Data.Text       as T
import qualified Data.Text.IO    as T

import qualified Distribution.PackageDescription as Cabal
import Text.XML.Light

import Merge.Utils as Merge
import Portage.EBuild as EBuild
import Portage.Metadata.RemoteId

import Hackport.Env

type UseFlags = Map.Map String String

-- | A data type for the Gentoo-specific @metadata.xml@ file.
-- Currently defines functions for the maintainer email and
-- USE flags and their descriptions.
data Metadata = Metadata
      { metadataEmails :: [String] -- ^ This should /always/ be [\"haskell@gentoo.org\"].
      , metadataUseFlags :: UseFlags -- ^ Only /active/ USE flags, if any.
      , metadataRemoteIds :: S.Set RemoteId
      } deriving (Eq, Show, Ord)

instance Semigroup Metadata where
  Metadata e1 u1 r1 <> Metadata e2 u2 r2 = Metadata (e1<>e2) (u1<>u2) (r1<>r2)

instance Monoid Metadata where
  mempty = Metadata mempty mempty mempty


-- | Update any existing metadata with changes to USE flags and remote-ids. If
--   the current metadata doesn't exist, it will modify a template created by
--   'minimalMetadata'.
updateMetadata :: WritesMetadata env 
    => env                 -- ^ Local environment carried by 'Env'
    -> EBuild.EBuild       -- ^ 'EBuild.EBuild' representation we are building metadata for
    -> [Cabal.PackageFlag] -- ^ cabal package flags
    -> Maybe Metadata      -- ^ Current metadata, if any
    -> Metadata
updateMetadata cmdEnv ebuild flags currentMeta =
  let
    -- Supply a default minimal metadata object if current metadata doesn't exist.
    currentMeta' = fromMaybe
      (minimalMetadata (useHackageRemote cmdEnv) ebuild)
      currentMeta

    -- The remote-id for hackage is always added, unless 'useHackageRemote' returns False
    -- (generally disabled on the command line with the --not-on-hackage flag for the
    -- make-ebuild command).
    hackageRemoteId =
      if useHackageRemote cmdEnv
        then S.singleton $ RemoteIdHackage $ EBuild.hackage_name ebuild
        else S.empty

    -- Sometimes the .cabal file will explicitly list source repos
    sourceRemoteIds =
      let r = matchURIs (EBuild.sourceURIs ebuild)
      in 
          if S.null r
            -- If the list of source remote-ids is empty, we fall back to using the homepage
            then matchURIs [EBuild.homepage ebuild]
            else r

    -- Create the new metadata, adding new USE flags (if any) to those of the
    -- existing metadata. If an existing flag has a new and old description,
    -- the new one takes precedence.
    in currentMeta' <> mempty
         { metadataUseFlags = Merge.metaFlags flags
         , metadataRemoteIds =
             hackageRemoteId <> sourceRemoteIds
         }

-- | Maybe return a 'Metadata' from a 'T.Text'.
--
-- Trying to parse an empty 'T.Text' should return 'Nothing':
--
-- >>> parseMetadataXML T.empty
-- Nothing
parseMetadataXML :: T.Text -> Maybe Metadata
parseMetadataXML = fmap parseMetadata . parseXMLDoc

-- | Apply 'parseMetadataXML' to a 'FilePath'.
readMetadataFile :: MonadIO m => FilePath -> m (Maybe Metadata)
readMetadataFile = liftIO . fmap parseMetadataXML . T.readFile

-- | Extract the maintainer email and USE flags from a supplied XML 'Element'.
-- 
-- If we're parsing a blank 'Element' or otherwise empty @metadata.xml@:
-- >>> parseMetadata blank_element
-- Metadata {metadataEmails = [], metadataUseFlags = fromList [], metadataRemoteIds = fromList []}
parseMetadata :: Element -> Metadata
parseMetadata xml =
  Metadata { metadataEmails = strContent <$> findElements (unqual "email") xml
           , metadataUseFlags =
               -- find the flag name
               let x = findElement (unqual "use") xml
                   y = onlyElems $ concatMap elContent x
                   z = attrVal <$> concatMap elAttribs y
               -- find the flag description
                   a = concatMap elContent y
                   b = cdData <$> onlyText a
               in Map.fromList $ zip z b
           -- TODO: Read remote-ids from existing metadata.xml
           , metadataRemoteIds = S.empty
           }

-- | Remove global @USE@ flags from the flags 'Map.Map', as these should not be
-- within the local @metadata.xml@. For now, this is manually specified rather than
-- parsing @use.desc@.
stripGlobalUseFlags :: UseFlags -> UseFlags
stripGlobalUseFlags m = foldr Map.delete m globals
  where
    globals = [ "debug"
              , "examples"
              , "static"
              , "test"
              ]

-- | Pretty print as valid XML a list of flags and their descriptions
-- from a given 'Map.Map'. This wraps the flags in "<use>...</use>" and is
-- meant to be passed to 'unlines'. If the Map is empty, it will return no
-- output.
prettyPrintFlags :: UseFlags -> [String]
prettyPrintFlags m
  | Map.null m = []
  | otherwise = ["\t<use>"] ++ go ++ ["\t</use>"]
  where
    go = printFlag <$> Map.toAscList m
    printFlag (n, d) =
      "\t\t" ++
      showElement
        (add_attr
          (Attr (blank_name { qName = "name" }) n)
          (unode "flag" d)
        )

-- | Pretty print a human-readable list of flags and their descriptions
-- from a given 'Map.Map'.
prettyPrintFlagsHuman :: UseFlags -> [String]
prettyPrintFlagsHuman m
  = (\(n,d) -> A.bold (n ++ ": ") ++ (unwords . lines $ d))
    <$> Map.toAscList m

-- | A minimal metadata for use as a fallback value.
minimalMetadata :: UseHackageRemote -> EBuild.EBuild -> Metadata
minimalMetadata useHackage ebuild =
  mempty
  { metadataEmails = ["haskell@gentoo.org"]
  , metadataRemoteIds =
      if useHackage
        then S.singleton $ RemoteIdHackage $ EBuild.hackage_name ebuild
        else S.empty
  }

-- don't use Text.XML.Light as we like our own pretty printer
-- | Pretty print the @metadata.xml@ string.
printMetadata :: Metadata -> T.Text
printMetadata (Metadata _ flags rids) = T.pack $
  unlines $
    [ "<?xml version=\"1.0\" encoding=\"UTF-8\"?>"
    , "<!DOCTYPE pkgmetadata SYSTEM \"https://www.gentoo.org/dtd/metadata.dtd\">"
    , "<pkgmetadata>"
    , "\t<maintainer type=\"project\">"
    , "\t\t<email>haskell@gentoo.org</email>"
    , "\t\t<name>Gentoo Haskell</name>"
    , "\t</maintainer>"
    ]
    ++ prettyPrintFlags (stripGlobalUseFlags flags)
    ++ prettyPrintRemoteIds rids
    ++
    [ "</pkgmetadata>"
    ]

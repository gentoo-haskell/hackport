{-# LANGUAGE CPP #-}

-- | Portage package identifiers, which unlike Cabal ones include a category.
--
module Portage.PackageId (
    Category(..),
    PackageName(..),
    PackageId(..),
    Portage.Version(..),
    mkPackageName,
    fromCabalPackageId,
    toCabalPackageId,
    parseFriendlyPackage,
    normalizeCabalPackageName,
    normalizeCabalPackageId,
    filePathToPackageId,
    packageIdToFilePath,
    cabal_pn_to_PN
  ) where

import Data.Char

import qualified Distribution.Package as Cabal
import Distribution.Text (Text(..))

import qualified Distribution.Compat.ReadP as Parse

import qualified Portage.Version as Portage

import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))
import qualified Data.Char as Char (isAlphaNum, isSpace, toLower)

import Distribution.Text(display)
import System.FilePath ( (</>) )

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

newtype Category = Category { unCategory :: String }
  deriving (Eq, Ord, Show, Read)

data PackageName = PackageName { category :: Category, cabalPkgName :: Cabal.PackageName }
  deriving (Eq, Ord, Show, Read)

data PackageId = PackageId { packageId :: PackageName, pkgVersion :: Portage.Version }
  deriving (Eq, Ord, Show, Read)

{-
instance Text PN where
  disp (PN n) = Disp.text n
  parse = do
    ns <- Parse.sepBy1 component (Parse.char '-')
    return (PN (concat (intersperse "-" ns)))
    where
      component = do
        cs <- Parse.munch1 Char.isAlphaNum
        if all Char.isDigit cs then Parse.pfail else return cs
        -- each component must contain an alphabetic character, to avoid
        -- ambiguity in identifiers like foo-1 (the 1 is the version number).
-}

packageIdToFilePath :: PackageId -> FilePath
packageIdToFilePath (PackageId (PackageName cat pn) version) =
  display cat </> display pn </> display pn <-> display version <.> "ebuild"
  where
    a <-> b = a ++ '-':b
    a <.> b = a ++ '.':b

-- This could be cleaned up with parsers.
-- | Attempt to generate a PackageId from a FilePath. If not, return
-- the provided PackageId as-is.
filePathToPackageId :: PackageId -> FilePath -> PackageId
filePathToPackageId pkgId fp = do
  -- take package name from provided FilePath
  let pn = take (length ((Cabal.unPackageName
                          . cabalPkgName
                          . packageId)
                         pkgId)) fp
      -- drop .ebuild file extension
      p = reverse . drop 1 $ dropWhile (/='.') $ reverse fp
      -- drop package name and the following dash
      v = drop ((length ((Cabal.unPackageName
                          . cabalPkgName
                          . packageId)
                         pkgId)) + 1) p
      c = unCategory . category . packageId $ pkgId
      -- parse and extract version
      parsed_v = case parseVersion v of
                   Just (Just my_v) -> my_v
                   _ -> pkgVersion pkgId
  -- Construct PackageId
  PackageId (mkPackageName c pn) parsed_v
  
mkPackageName :: String -> String -> PackageName
mkPackageName cat package = PackageName (Category cat) (Cabal.mkPackageName package)

fromCabalPackageId :: Category -> Cabal.PackageIdentifier -> PackageId
fromCabalPackageId category (Cabal.PackageIdentifier name version) =
  PackageId (PackageName category (normalizeCabalPackageName name))
            (Portage.fromCabalVersion version)

normalizeCabalPackageName :: Cabal.PackageName -> Cabal.PackageName
normalizeCabalPackageName =
  Cabal.mkPackageName . map Char.toLower . Cabal.unPackageName

normalizeCabalPackageId :: Cabal.PackageIdentifier -> Cabal.PackageIdentifier
normalizeCabalPackageId (Cabal.PackageIdentifier name version) =
  Cabal.PackageIdentifier (normalizeCabalPackageName name) version

toCabalPackageId :: PackageId -> Maybe Cabal.PackageIdentifier
toCabalPackageId (PackageId (PackageName _cat name) version) =
  fmap (Cabal.PackageIdentifier name)
           (Portage.toCabalVersion version)

instance Text Category where
  disp (Category c) = Disp.text c
  parse = fmap Category (Parse.munch1 categoryChar)
    where
      categoryChar c = Char.isAlphaNum c || c == '-'

instance Text PackageName where
  disp (PackageName category name) =
    disp category <> Disp.char '/' <> disp name

  parse = do
    category <- parse
    _ <- Parse.char '/'
    name <- parse
    return (PackageName category name)

instance Text PackageId where
  disp (PackageId name version) =
    disp name <> Disp.char '-' <> disp version

  parse = do
    name <- parse
    _ <- Parse.char '-'
    version <- parse
    return (PackageId name version)

parseFriendlyPackage :: String -> Maybe (Maybe Category, Cabal.PackageName, Maybe Portage.Version)
parseFriendlyPackage str =
  case [ p | (p,s) <- Parse.readP_to_S parser str
       , all Char.isSpace s ] of
    [] -> Nothing
    (x:_) -> Just x
  where
  parser = do
    mc <- Parse.option Nothing $ do
      c <- parse
      _ <- Parse.char '/'
      return (Just c)
    p <- parse
    mv <- Parse.option Nothing $ do
      _ <- Parse.char '-'
      v <- parse
      return (Just v)
    return (mc, p, mv)

-- | Parse a String in the form of a Portage version
parseVersion :: FilePath -> Maybe (Maybe Portage.Version)
parseVersion str =
    case [ p | (p,s) <- Parse.readP_to_S parser str
           , all Char.isSpace s ] of
      [] -> Nothing
      (x:_) -> Just x
    where
      parser = do
        mv <- Parse.option Nothing $ do
                 v <- parse
                 return (Just v)
        return mv
    
cabal_pn_to_PN :: Cabal.PackageName -> String
cabal_pn_to_PN = map toLower . display

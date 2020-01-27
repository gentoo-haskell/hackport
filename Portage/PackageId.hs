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

import qualified Distribution.Package as Cabal

import Distribution.Parsec.Class (CabalParsing(..), Parsec(..), explicitEitherParsec)
import qualified Distribution.Compat.CharParsing as P

import qualified Portage.Version as Portage

import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))
import qualified Data.Char as Char (isAlphaNum, toLower)

import Distribution.Pretty (Pretty(..), prettyShow)
import System.FilePath ((</>), dropExtension)

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

newtype Category = Category { unCategory :: String }
  deriving (Eq, Ord, Show, Read)

data PackageName = PackageName { category :: Category, cabalPkgName :: Cabal.PackageName }
  deriving (Eq, Ord, Show, Read)

data PackageId = PackageId { packageId :: PackageName, pkgVersion :: Portage.Version }
  deriving (Eq, Ord, Show, Read)

instance Pretty Category where
  pretty (Category c) = Disp.text c

instance Parsec Category where
  parsec = Category <$> P.munch1 categoryChar
    where
      categoryChar c = Char.isAlphaNum c || c == '-'

instance Pretty PackageName where
  pretty (PackageName category name) =
    pretty category <> Disp.char '/' <> pretty name

instance Parsec PackageName where
  parsec = do
    category <- parsec
    _ <- P.char '/'
    name <- parsec
    return $ PackageName category name

instance Pretty PackageId where
  pretty (PackageId name version) =
    pretty name <> Disp.char '-' <> pretty version

instance Parsec PackageId where
  parsec = do
    name <- parsec
    _ <- P.char '-'
    version <- parsec
    return $ PackageId name version

packageIdToFilePath :: PackageId -> FilePath
packageIdToFilePath (PackageId (PackageName cat pn) version) =
  prettyShow cat </> prettyShow pn </> prettyShow pn <-> prettyShow version <.> "ebuild"
  where
    a <-> b = a ++ '-':b
    a <.> b = a ++ '.':b

-- | Attempt to generate a PackageId from a FilePath. If not, return
-- the provided PackageId as-is.
--
-- TODO: rewrite this function using Parsec.
filePathToPackageId :: PackageId -> FilePath -> PackageId
filePathToPackageId pkgId fp = do
      -- take package name from provided FilePath
  let pn = take (length
                 $ Cabal.unPackageName . cabalPkgName . packageId
                 $ pkgId) fp
      -- drop .ebuild file extension
      p = dropExtension fp
      -- drop package name and the following dash
      v = drop ((length pn) +1) p
      c = unCategory . category . packageId $ pkgId
      -- parse and extract version
      parsed_v = case parseVersion v of
                   Just my_v -> my_v
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

parseFriendlyPackage :: String -> Either String (Maybe Category, Cabal.PackageName, Maybe Portage.Version)
parseFriendlyPackage str = explicitEitherParsec parser str
  where
  parser = do
    mc <- P.optional . P.try $ do
      c <- parsec
      _ <- P.char '/'
      return c
    p <- parseCabalPackageName
    mv <- P.optional $ do
      _ <- P.char '-'
      v <- parsec
      return v
    return (mc, p, mv)

-- | Parse a Cabal PackageName. Note that we cannot use the @parsec@
-- method as defined in the @Parsec PackageName@ instance, since it
-- fails the entire PackageName parse if it tries to parse a version
-- number.
parseCabalPackageName :: CabalParsing m => m Cabal.PackageName
parseCabalPackageName = do
  pn <- P.some . P.try $
    P.choice
    [ P.alphaNum
    , P.char '-' <* P.notFollowedBy (P.some P.digit <* P.notFollowedBy (P.some P.letter))
    ]
  return $ Cabal.mkPackageName pn

-- | Parse a String in the form of a Portage version
parseVersion :: FilePath -> Maybe Portage.Version
parseVersion str =
  case explicitEitherParsec parser str of
    Right x -> x
    Left _ -> Nothing
    where
      parser = do
        mv <- P.optional $ do
          v <- parsec
          return v
        return mv

cabal_pn_to_PN :: Cabal.PackageName -> String
cabal_pn_to_PN = map Char.toLower . prettyShow

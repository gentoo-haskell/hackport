{-# LANGUAGE CPP #-}
{-|
Module      : Portage.PackageId
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Portage package identifiers, which unlike Cabal ones include a category.
-}
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

import qualified Distribution.Compat.CharParsing as P
import qualified Distribution.Package as Cabal
import           Distribution.Parsec (CabalParsing(..), Parsec(..), explicitEitherParsec)
import           Distribution.Pretty (Pretty(..), prettyShow)

import qualified Portage.Version as Portage

import qualified Data.Char as Char
import qualified Text.PrettyPrint as Disp
import           Text.PrettyPrint ((<>))
import           System.FilePath ((</>))

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

newtype Category = Category { unCategory :: String }
  deriving (Eq, Ord, Show, Read)

-- | Portage-style 'PackageName', containing a 'Category' and a 'Cabal.PackageName'.
data PackageName = PackageName { category :: Category, cabalPkgName :: Cabal.PackageName }
  deriving (Eq, Ord, Show, Read)

-- | Portage-style 'PackageId', containing a 'PackageName' and a 'Portage.Version'.
data PackageId = PackageId { pkgName :: PackageName, pkgVersion :: Portage.Version }
  deriving (Eq, Ord, Show, Read)

instance Pretty Category where
  pretty (Category c) = Disp.text c

instance Parsec Category where
  parsec = Category <$> P.munch1 categoryChar
    where
      categoryChar c = Char.isAlphaNum c || c == '-'

instance Pretty PackageName where
  pretty (PackageName cat name) =
    pretty cat <> Disp.char '/' <> pretty name

instance Parsec PackageName where
  parsec = do
    cat <- parsec
    _ <- P.char '/'
    name <- parseCabalPackageName
    return $ PackageName cat name

instance Pretty PackageId where
  pretty (PackageId name version) =
    pretty name <> Disp.char '-' <> pretty version

instance Parsec PackageId where
  parsec = do
    name <- parsec
    _ <- P.char '-'
    version <- parsec
    return $ PackageId name version

-- | Transform a 'PackageId' into a 'FilePath'.
-- 
-- >>> packageIdToFilePath (PackageId (PackageName (Category "dev-haskell") (Cabal.mkPackageName "foo-bar2")) (Portage.Version [3,0,0] (Just 'b') [Portage.RC 2] 1 ))
-- "dev-haskell/foo-bar2/foo-bar2-3.0.0b_rc2-r1.ebuild"
packageIdToFilePath :: PackageId -> FilePath
packageIdToFilePath (PackageId (PackageName cat pn) version) =
  prettyShow cat </> prettyShow pn </> prettyShow pn <-> prettyShow version <.> "ebuild"
  where
    a <-> b = a ++ '-':b
    a <.> b = a ++ '.':b

-- | Maybe generate a 'PackageId' from a 'FilePath'. Note that the 'FilePath' must have its
-- file extension stripped before being passed to 'filePathToPackageId'.
-- 
-- >>> filePathToPackageId (Category "dev-haskell") "foo-bar2-3.0.0b_rc2-r1"
-- Just (PackageId {pkgName = PackageName {category = Category {unCategory = "dev-haskell"}, cabalPkgName = PackageName "foo-bar2"}, pkgVersion = Version {versionNumber = [3,0,0], versionChar = Just 'b', versionSuffix = [RC 2], versionRevision = 1}})
filePathToPackageId :: Category -> FilePath -> Maybe PackageId
filePathToPackageId cat fp =
  case explicitEitherParsec parser fp of
    Right x -> Just x
    _ -> Nothing
  where
    parser = do
      pn <- parseCabalPackageName
      _ <- P.char '-'
      v <- parsec
      return $ PackageId (PackageName cat pn) v

-- | Create a 'PackageName' from supplied category and package name 'String's.
mkPackageName :: String -> String -> PackageName
mkPackageName cat package = PackageName (Category cat) (Cabal.mkPackageName package)

-- | Create a 'PackageId' from a 'Category' and 'Cabal.PackageIdentifier'.
fromCabalPackageId :: Category -> Cabal.PackageIdentifier -> PackageId
fromCabalPackageId cat (Cabal.PackageIdentifier name version) =
  PackageId (PackageName cat (normalizeCabalPackageName name))
            (Portage.fromCabalVersion version)

-- | Convert a 'Cabal.PackageName' into lowercase. Internally uses
-- 'cabal_pn_to_PN'.
--
-- >>> normalizeCabalPackageName (Cabal.mkPackageName "FooBar1")
-- PackageName "foobar1"
normalizeCabalPackageName :: Cabal.PackageName -> Cabal.PackageName
normalizeCabalPackageName =
  Cabal.mkPackageName . cabal_pn_to_PN

-- | Apply 'normalizeCabalPackageName' to the 'Cabal.PackageName' of
-- a supplied 'Cabal.PackageIdentifier'.
normalizeCabalPackageId :: Cabal.PackageIdentifier -> Cabal.PackageIdentifier
normalizeCabalPackageId (Cabal.PackageIdentifier name version) =
  Cabal.PackageIdentifier (normalizeCabalPackageName name) version

-- | Convert a 'PackageId' into a 'Maybe' 'Cabal.PackageIdentifier'.
toCabalPackageId :: PackageId -> Maybe Cabal.PackageIdentifier
toCabalPackageId (PackageId (PackageName _cat name) version) =
  fmap (Cabal.PackageIdentifier name)
           (Portage.toCabalVersion version)

-- | Parse a 'String' as a package in the form of @[category\/]name[-version]@:
--
-- Note that we /cannot/ use the 'parsec' function to parse the 'Cabal.PackageName',
-- since it fails the entire parse if it tries to parse a 'Version'.
-- See 'parseCabalPackageName' below.
--
-- If parsing a valid package string:
-- 
-- >>> parseFriendlyPackage "category-name/package-name1-0.0.0.1a_beta2-r4"
-- Right (Just (Category {unCategory = "category-name"}),PackageName "package-name1",Just (Version {versionNumber = [0,0,0,1], versionChar = Just 'a', versionSuffix = [Beta 2], versionRevision = 4}))
--
-- If malformed, return an error string:
--
-- >>> parseFriendlyPackage "category-name/package-name-1-0.0.0.1a_beta2-r4"
-- Left ...
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

-- | Parse a 'Cabal.PackageName'.
--
-- This parser is a replacement for 'parsecUnqualComponentName' which fails when
-- trying to parse a 'Version'.
parseCabalPackageName :: CabalParsing m => m Cabal.PackageName
parseCabalPackageName = do
  pn <- P.some . P.try $
    P.choice
    [ P.alphaNum
    , P.char '+'
    , P.char '-' <* P.notFollowedBy (P.some P.digit <* P.notFollowedBy P.letter)
    ]
  return $ Cabal.mkPackageName pn

-- | Pretty-print a lowercase 'Cabal.PackageName'.
--
-- Note the difference between this function and 'normalizeCabalPackageName':
-- this function returns a 'String', the other a 'Cabal.PackageName'.
--
-- >>> cabal_pn_to_PN (Cabal.mkPackageName "FooBar1")
-- "foobar1"
cabal_pn_to_PN :: Cabal.PackageName -> String
cabal_pn_to_PN = map Char.toLower . prettyShow

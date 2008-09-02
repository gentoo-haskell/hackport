-- | Portage package identifiers, which unlike Cabal ones include a category.
--
module Portage.PackageId (
    Category(..),
    PackageName(..),
    PackageId(..),
    fromCabalPackageId,
    toCabalPackageId,
  ) where

import qualified Portage.Version as Portage

import qualified Distribution.Package as Cabal
import Distribution.Text (Text(..))

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))
import qualified Data.Char as Char (isAlphaNum, toLower)


newtype Category = Category String
  deriving (Eq, Ord, Show, Read)

data PackageName = PackageName Category Cabal.PackageName
  deriving (Eq, Ord, Show, Read)

data PackageId = PackageId PackageName Portage.Version
  deriving (Eq, Ord, Show, Read)

fromCabalPackageId :: Category -> Cabal.PackageIdentifier -> PackageId
fromCabalPackageId category (Cabal.PackageIdentifier name version) =
  PackageId (PackageName category (lowercase name))
            (Portage.fromCabalVersion version)
  where
    lowercase (Cabal.PackageName n) = Cabal.PackageName (map Char.toLower n)

toCabalPackageId :: PackageId -> Maybe Cabal.PackageIdentifier
toCabalPackageId (PackageId (PackageName _cat name) version) =
  fmap (Cabal.PackageIdentifier name) (Portage.toCabalVersion version)

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
    Parse.char '/'
    name <- parse
    return (PackageName category name)

instance Text PackageId where
  disp (PackageId name version) =
    disp name <> Disp.char '-' <> disp version

  parse = do
    name <- parse
    Parse.char '-'
    version <- parse
    return (PackageId name version)

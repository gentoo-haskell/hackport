-- | Portage package identifiers, which unlike Cabal ones include a category.
--
module Portage.PackageId (
    Category(..),
    PackageName(..),
    PackageId(..),
  ) where

import qualified Distribution.Package as Cabal
import Distribution.Version
import Distribution.Text (Text(..))

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))
import qualified Data.Char as Char ( isAlphaNum )


newtype Category = Category String
  deriving (Eq, Ord, Show, Read)

data PackageName = PackageName Category Cabal.PackageName
  deriving (Eq, Ord, Show, Read)

data PackageId = PackageId PackageName Version
  deriving (Eq, Ord, Show, Read)

instance Cabal.Package PackageId where
  packageId (PackageId (PackageName _ n) v) = Cabal.PackageIdentifier n v

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

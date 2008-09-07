-- | Portage package identifiers, which unlike Cabal ones include a category.
--
module Portage.PackageId (
    Category(..),
    PackageName(..),
    PackageId(..),
    PN(..),
    fromCabalPackageId,
    toCabalPackageId,
    parseFriendlyPackage
  ) where

import qualified Portage.Version as Portage

import qualified Distribution.Package as Cabal
import Distribution.Text (Text(..),display)

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))
import qualified Data.Char as Char (isAlphaNum, isDigit, isSpace, toLower)
import Data.List (intersperse)

newtype Category = Category String
  deriving (Eq, Ord, Show, Read)

data PackageName = PackageName Category PN
  deriving (Eq, Ord, Show, Read)

data PackageId = PackageId PackageName Portage.Version
  deriving (Eq, Ord, Show, Read)

data PN = PN String -- replace with Cabal.PackageName once we use Cabal>=1.5
  deriving (Eq, Ord, Show, Read)

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

fromCabalPackageId :: Category -> Cabal.PackageIdentifier -> PackageId
fromCabalPackageId category (Cabal.PackageIdentifier name version) =
  PackageId (PackageName category (PN (lowercase name)))
            (Portage.fromCabalVersion version)
  where
    lowercase = map Char.toLower

toCabalPackageId :: PackageId -> Maybe Cabal.PackageIdentifier
toCabalPackageId (PackageId (PackageName _cat (PN name)) version) =
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

parseFriendlyPackage :: String -> Maybe (Maybe Category, PN, Maybe Portage.Version)
parseFriendlyPackage str =
  case [ p | (p,s) <- Parse.readP_to_S parser str
       , all Char.isSpace s ] of
    [] -> Nothing
    (x:_) -> Just x
  where
  parser = do
    mc <- Parse.option Nothing $ do
      c <- parse
      Parse.char '/'
      return (Just c)
    p <- parse
    mv <- Parse.option Nothing $ do
      Parse.char '-'
      v <- parse
      return (Just v)
    return (mc, p, mv)
      

{-|
Module      : Merge.Utils
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Internal helper functions for "Merge".
-}
module Merge.Utils
  ( readPackageString
  , getPreviousPackageId
  , first_just_of
  , drop_prefix
  , mangle_iuse
  , to_unstable
  , metaFlags
  , gentooPackages
  ) where

import qualified Control.Monad as M
import           Data.Maybe (mapMaybe)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified System.FilePath as SF

import           Error
import qualified Portage.PackageId as Portage

import qualified Distribution.Package            as Cabal
import qualified Distribution.PackageDescription as Cabal

-- | Parse a ['String'] as a valid package string. E.g. @category\/name-1.0.0@.
-- Return 'HackPortError' if the string to parse is invalid.
--
-- When the ['String'] is valid:
--
-- >>> readPackageString ["dev-haskell/packagename1-1.0.0"]
-- Right (Just (Category {unCategory = "dev-haskell"}),PackageName "packagename1",Just (Version {versionNumber = [1,0,0], versionChar = Nothing, versionSuffix = [], versionRevision = 0}))
--
-- When the ['String'] is empty:
--
-- >>> readPackageString []
-- Left ...
readPackageString :: [String]
                  -> Either HackPortError ( Maybe Portage.Category
                                          , Cabal.PackageName
                                          , Maybe Portage.Version
                                          )
readPackageString args = do
  packageString <-
    case args of
      [] -> Left (ArgumentError "Need an argument, [category/]package[-version]")
      [pkg] -> return pkg
      _ -> Left (ArgumentError ("Too many arguments: " ++ unwords args))
  case Portage.parseFriendlyPackage packageString of
    Right v@(_,_,Nothing) -> return v
    -- we only allow versions we can convert into cabal versions
    Right v@(_,_,Just (Portage.Version _ Nothing [] 0)) -> return v
    Left e -> Left $ ArgumentError $ "Could not parse [category/]package[-version]: "
              ++ packageString ++ "\nParsec error: " ++ e
    _ -> Left $ ArgumentError $ "Could not parse [category/]package[-version]: "
         ++ packageString

-- | Maybe return a 'Portage.PackageId' of the next highest version for a given
--   package, relative to the provided 'Portage.PackageId'.
--
-- For example:
-- 
-- >>> let ebuildDir = ["foo-bar2-3.0.1.ebuild","metadata.xml"]
-- >>> let newPkgId = Portage.PackageId (Portage.PackageName (Portage.Category "dev-haskell") (Cabal.mkPackageName "foo-bar2")) (Portage.Version [3,0,2] Nothing [] 0 )
--
-- >>> getPreviousPackageId ebuildDir newPkgId
-- Just (PackageId {pkgName = PackageName {category = Category {unCategory = "dev-haskell"}, cabalPkgName = PackageName "foo-bar2"}, pkgVersion = Version {versionNumber = [3,0,1], versionChar = Nothing, versionSuffix = [], versionRevision = 0}})
getPreviousPackageId :: [FilePath] -- ^ list of ebuilds for given package
                     -> Portage.PackageId -- ^ new PackageId
                     -> Maybe Portage.PackageId -- ^ maybe PackageId of previous version
getPreviousPackageId pkgDir newPkgId = do
  let pkgIds = reverse 
               . L.sortOn (Portage.pkgVersion)
               . filter (<newPkgId)
               $ mapMaybe (Portage.filePathToPackageId (Portage.category . Portage.pkgName $ newPkgId))
               $ SF.dropExtension <$> filter (\fp -> SF.takeExtension fp == ".ebuild") pkgDir
  case pkgIds of
    x:_ -> Just x
    _ -> Nothing

-- | Alias for 'msum'.
-- 
-- prop> \a -> first_just_of a == M.msum a
first_just_of :: [Maybe a] -> Maybe a
first_just_of = M.msum

-- | Remove @with@ or @use@ prefixes from flag names.
--
-- >>> drop_prefix "with_conduit"
-- "conduit"
-- >>> drop_prefix "use-https"
-- "https"
-- >>> drop_prefix "no_examples"
-- "no_examples"
drop_prefix :: String -> String
drop_prefix x
  | take 5 x `elem` ["with_","with-"] = drop 5 x
  | take 4 x `elem` ["use_","use-"]   = drop 4 x
  | otherwise = x

-- | Gentoo allows underscore ('_') names in @IUSE@ only for
-- @USE_EXPAND@ values. If it's not a user-specified rename mangle
-- it into a hyphen ('-').
-- 
-- >>> mangle_iuse "use_remove_my_underscores"
-- "remove-my-underscores"
mangle_iuse :: String -> String
mangle_iuse = drop_prefix . map f
  where f '_' = '-'
        f c   = c

-- | Convert all stable keywords to testing (unstable) keywords.
-- Preserve arch masks (-).
--
-- >>> to_unstable "amd64"
-- "~amd64"
-- >>> to_unstable "~amd64"
-- "~amd64"
-- >>> to_unstable "-amd64"
-- "-amd64"
to_unstable :: String -> String
to_unstable kw =
    case kw of
        '~':_ -> kw
        '-':_ -> kw
        _     -> '~':kw

-- | Generate a 'Map.Map' of 'Cabal.PackageFlag' names and their descriptions.
--
-- For example, if we construct a singleton list holding a 'Cabal.PackageFlag' with
-- 'Cabal.FlagName' @foo@ and 'Cabal.FlagDescription' @bar@, we should get
-- a 'Map.Map' containing those values:
--
-- >>> let flags = [(Cabal.emptyFlag (Cabal.mkFlagName "foo")) {Cabal.flagDescription = "bar"}]
-- >>> metaFlags flags
-- fromList [("foo","bar")]
metaFlags :: [Cabal.PackageFlag] -> Map.Map String String
metaFlags flags = Map.fromList $ zip (mangle_iuse . Cabal.unFlagName . Cabal.flagName <$> flags) (Cabal.flagDescription <$> flags)

-- | List of packages actively maintained in @::gentoo@.
--
-- When merging a package, hackport checks if the package exists
-- on this list. If it does, it advises the user to sync their
-- package bump to ::gentoo.
--
-- GHC and its core libraries are currently commented out, since their
-- management requires coordination with the rest of the haskell
-- ecosystem and should not be bumped ad-hoc.
gentooPackages :: [Portage.PackageName]
gentooPackages = [ p "app-portage" "hackport"
                 , p "app-text" "pandoc"
                 -- , p "dev-haskell" "binary"       -- ghc core library
                 -- , p "dev-haskell" "cabal"        -- ghc core library
                 , p "dev-haskell" "cabal-install"
                 -- , p "dev-haskell" "exceptions"   -- ghc-8.10 core library
                 , p "dev-haskell" "haddock"
                 , p "dev-haskell" "hakyll"
                 -- , p "dev-haskell" "haskeline"    -- ghc core library
                 , p "dev-haskell" "hlint"
                 -- , p "dev-haskell" "mtl"          -- ghc core library
                 -- , p "dev-haskell" "parsec"       -- ghc core library
                 , p "dev-haskell" "stack"
                 , p "dev-haskell" "stack-bin"
                 -- , p "dev-haskell" "stm"          -- ghc core library
                 -- , p "dev-haskell" "terminfo"     -- ghc core library
                 -- , p "dev-haskell" "text"        
                 -- , p "dev-haskell" "transformers" -- ghc core library
                 -- , p "dev-haskell" "xhtml"        -- ghc core library
                 , p "dev-haskell" "yesod"
                 -- , p "dev-lang" "ghc"             -- ghc itself
                 , p "dev-vcs" "darcs"
                 , p "dev-vcs" "git-annex"
                 , p "x11-misc" "xmobar"
                 , p "x11-wm" "xmonad"
                 ]
  where
    p = Portage.mkPackageName

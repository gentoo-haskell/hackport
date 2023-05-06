{-|
Module      : Merge.Utils
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Internal helper functions for "Merge".
-}
module Merge.Utils
  ( readPackageString
  , getPreviousPackageId
  , drop_prefix
  , squash_debug
  , convert_underscores
  , mangle_iuse
  , to_unstable
  , metaFlags
  , dropIfUseExpands
  -- hspec exports
  , dropIfUseExpand
  ) where

import qualified Control.Applicative as A
import qualified Data.Char as C
import           Data.Maybe (catMaybes, mapMaybe)
import qualified Data.List as L
import qualified Data.Map.Strict as Map
import qualified System.Directory as SD
import qualified System.FilePath as SF
import           System.FilePath ((</>))
import           System.Process (readCreateProcess, shell)
import           Error
import qualified Portage.PackageId as Portage

import qualified Distribution.Package            as Cabal
import qualified Distribution.PackageDescription as Cabal

-- | Parse a 'String' as a valid package string. E.g. @category\/name-1.0.0@.
-- Return 'HackPortError' if the string to parse is invalid.
--
-- When the 'String' is valid:
--
-- >>> readPackageString "dev-haskell/packagename1-1.0.0"
-- Right (Just (Category {unCategory = "dev-haskell"}),PackageName "packagename1",Just (Version {versionNumber = [1,0,0], versionChar = Nothing, versionSuffix = [], versionRevision = 0}))
readPackageString :: String
                  -> Either HackPortError ( Maybe Portage.Category
                                          , Cabal.PackageName
                                          , Maybe Portage.Version
                                          )
readPackageString packageString = do
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
-- Just (PackageId {packageId = PackageName {category = Category {unCategory = "dev-haskell"}, cabalPkgName = PackageName "foo-bar2"}, pkgVersion = Version {versionNumber = [3,0,1], versionChar = Nothing, versionSuffix = [], versionRevision = 0}})
getPreviousPackageId :: [FilePath] -- ^ list of ebuilds for given package
                     -> Portage.PackageId -- ^ new PackageId
                     -> Maybe Portage.PackageId -- ^ maybe PackageId of previous version
getPreviousPackageId pkgDir newPkgId = do
  let pkgIds = reverse 
               . L.sortOn (Portage.pkgVersion)
               . filter (<newPkgId)
               $ mapMaybe (Portage.filePathToPackageId (Portage.category . Portage.packageId $ newPkgId))
               $ SF.dropExtension <$> filter (\fp -> SF.takeExtension fp == ".ebuild") pkgDir
  case pkgIds of
    x:_ -> Just x
    _ -> Nothing

-- | Remove @with@ or @use@ prefixes from flag names.
--
-- >>> drop_prefix "with_conduit"
-- "conduit"
-- >>> drop_prefix "use-https"
-- "https"
-- >>> drop_prefix "no_examples"
-- "no_examples"
drop_prefix :: String -> String
drop_prefix x =
  let prefixes = ["with","use"]
      separators = ["-","_"]
      combinations = A.liftA2 (++) prefixes separators
  in case catMaybes (A.liftA2 L.stripPrefix combinations [x]) of
    [z] -> z
    _ -> x

-- | Squash debug-related @USE@ flags under the @debug@ global
--   @USE@ flag.
--
-- >>> squash_debug "use-debug-foo"
-- "debug"
-- >>> squash_debug "foo-bar"
-- "foo-bar"
squash_debug :: String -> String
squash_debug flag = if "debug" `L.isInfixOf` (C.toLower <$> flag)
                         then "debug"
                         else flag

-- | Gentoo allows underscore ('_') names in @IUSE@ only for
-- @USE_EXPAND@ values. If it's not a user-specified rename mangle
-- it into a hyphen ('-').
-- 
-- >>> convert_underscores "remove_my_underscores"
-- "remove-my-underscores"
convert_underscores :: String -> String
convert_underscores = map f
  where f '_' = '-'
        f c   = c

-- | Perform all @IUSE@ mangling.
--
-- >>> mangle_iuse "use_foo-bar_debug"
-- "debug"
-- >>> mangle_iuse "with-bar_quux"
-- "bar-quux"
mangle_iuse :: String -> String
mangle_iuse = squash_debug . drop_prefix . convert_underscores

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
metaFlags flags =
  Map.fromList $
  zip (mangle_iuse . Cabal.unFlagName . Cabal.flagName <$> flags)
  (Cabal.flagDescription <$> flags)

-- | Return a list of @USE_EXPAND@s maintained by ::gentoo.
--
-- First, 'getUseExpands' runs @portageq@ to determine the 'FilePath' of the
-- directory containing valid @USE_EXPAND@s. If the 'FilePath' exists,
-- it drops the filename extensions to return a list of @USE_EXPAND@s
-- as Portage understands them. If the 'FilePath' does not exist, 'getUseExpands'
-- supplies a bare-bones list of @USE_EXPAND@s.
getUseExpands :: IO [String]
getUseExpands = do
  portDir <- readCreateProcess (shell "portageq get_repo_path / gentoo") ""
  let use_expands_dir = (L.dropWhileEnd C.isSpace portDir) </> "profiles" </> "desc"
  path_exists <- SD.doesPathExist use_expands_dir
  if path_exists
    then do use_expands_contents <- SD.listDirectory use_expands_dir
            return (SF.dropExtension <$> use_expands_contents)
    -- Provide some sensible defaults if hackport cannot find ::gentoo
    else let use_expands_contents = ["cpu_flags_arm","cpu_flags_ppc","cpu_flags_x86"]
         in return use_expands_contents

-- | Return a 'Cabal.PackageFlag' if it is not a @USE_EXPAND@.
--
-- If the 'Cabal.flagName' has a prefix matching any valid @USE_EXPAND@,
-- then return 'Nothing'. Otherwise return 'Just' 'Cabal.PackageFlag'.
dropIfUseExpand :: [String] -> Cabal.PackageFlag -> Maybe Cabal.PackageFlag
dropIfUseExpand use_expands flag =
  if or (A.liftA2 L.isPrefixOf use_expands [Cabal.unFlagName . Cabal.flagName $ flag])
  then Nothing else Just flag

-- | Strip @USE_EXPAND@s from a ['Cabal.PackageFlag'].
dropIfUseExpands :: [Cabal.PackageFlag] -> IO [Cabal.PackageFlag]
dropIfUseExpands flags = do
  use_expands <- getUseExpands
  return $ catMaybes (dropIfUseExpand use_expands <$> flags)

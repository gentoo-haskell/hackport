{-# OPTIONS -XPatternGuards #-}
module Merge
  ( merge
  , mergeGenericPackageDescription
  ) where

import Control.Monad.Error
import Control.Exception
import Data.Maybe
import Data.List as L
import Distribution.Package
import Distribution.PackageDescription ( PackageDescription(..)
                                       , FlagName(..)
                                       , GenericPackageDescription
                                       )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Text (display)

import System.Directory ( getCurrentDirectory
                        , setCurrentDirectory
                        , createDirectoryIfMissing
                        , doesFileExist
                        )
import System.Cmd (system)
import System.FilePath ((</>))
import System.Exit

import qualified Cabal2Ebuild as C2E
import qualified Portage.EBuild as E
import Error as E

import qualified Distribution.Package as Cabal
import qualified Distribution.Version as Cabal

import Distribution.System (buildPlatform)
import Distribution.Verbosity
import Distribution.Simple.Utils

import Network.URI

import Distribution.Client.IndexUtils ( getSourcePackages )
import qualified Distribution.Client.PackageIndex as Index
import Distribution.Client.Types

import qualified Portage.PackageId as Portage
import qualified Portage.Version as Portage
import qualified Portage.Metadata as Portage
import qualified Portage.Overlay as Overlay
import qualified Portage.Resolve as Portage

import qualified Portage.GHCCore as GHCCore

import qualified Merge.Dependencies as Merge

import Debug.Trace ( trace )

(<.>) :: String -> String -> String
a <.> b = a ++ '.':b

{-
Requested features:
  * Copy the old keywords and ~arch them
  * Add files to darcs?
  * Print diff with the next latest version?
BUGS:
  * Dependencies are always expected to be in dev-haskell
-}

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
    Just v@(_,_,Nothing) -> return v
    -- we only allow versions we can convert into cabal versions
    Just v@(_,_,Just (Portage.Version _ Nothing [] 0)) -> return v
    _ -> Left (ArgumentError ("Could not parse [category/]package[-version]: " ++ packageString))



-- | Given a list of available packages, and maybe a preferred version,
-- return the available package with that version. Latest version is chosen
-- if no preference.
resolveVersion :: [SourcePackage] -> Maybe Cabal.Version -> Maybe SourcePackage
resolveVersion avails Nothing = Just $ maximumBy (comparing packageInfoId) avails
resolveVersion avails (Just ver) = listToMaybe (filter match avails)
  where
    match avail = ver == pkgVersion (packageInfoId avail)

merge :: Verbosity -> Repo -> URI -> [String] -> FilePath -> IO ()
merge verbosity repo _serverURI args overlayPath = do
  (m_category, user_pName, m_version) <-
    case readPackageString args of
      Left err -> throwEx err
      Right (c,p,m_v) ->
        case m_v of
          Nothing -> return (c,p,Nothing)
          Just v -> case Portage.toCabalVersion v of
                      Nothing -> throwEx (ArgumentError "illegal version")
                      Just ver -> return (c,p,Just ver)

  debug verbosity $ "Category: " ++ show m_category
  debug verbosity $ "Package: " ++ show user_pName
  debug verbosity $ "Version: " ++ show m_version

  let (Cabal.PackageName user_pname_str) = user_pName

  overlay <- Overlay.loadLazy overlayPath
  -- portage_path <- Host.portage_dir `fmap` Host.getInfo
  -- portage <- Overlay.loadLazy portage_path
  index <- fmap packageIndex $ getSourcePackages verbosity [ repo ]

  -- find all packages that maches the user specified package name
  availablePkgs <-
    case map snd (Index.searchByName index user_pname_str) of
      [] -> throwEx (PackageNotFound user_pname_str)
      [pkg] -> return pkg
      pkgs  -> let names      = map (pkgName . packageInfoId . L.head) pkgs
                   whole_list = map (L.intercalate "\n" . map (show . packageInfoId)) pkgs
               in throwEx $ ArgumentError $ L.intercalate "\n---\n" $ ["Ambiguous names: " ++ show names] ++ whole_list

  -- select a single package taking into account the user specified version
  selectedPkg <-
    case resolveVersion availablePkgs m_version of
      Nothing -> do
        putStrLn "No such version for that package, available versions:"
        forM_ availablePkgs $ \ avail ->
          putStrLn (display . packageInfoId $ avail)
        throwEx (ArgumentError "no such version for that package")
      Just avail -> return avail

  -- print some info
  info verbosity "Selecting package:"
  forM_ availablePkgs $ \ avail -> do
    let match_text | packageInfoId avail == packageInfoId selectedPkg = "* "
                   | otherwise = "- "
    info verbosity $ match_text ++ (display . packageInfoId $ avail)

  let cabal_pkgId = packageInfoId selectedPkg
      norm_pkgName = packageName (Portage.normalizeCabalPackageId cabal_pkgId)
  cat <- maybe (Portage.resolveCategory verbosity overlay norm_pkgName) return m_category
  mergeGenericPackageDescription verbosity overlayPath cat (packageDescription selectedPkg) True

mergeGenericPackageDescription :: Verbosity -> FilePath -> Portage.Category -> GenericPackageDescription -> Bool -> IO ()
mergeGenericPackageDescription verbosity overlayPath cat pkgGenericDesc fetch = do
  overlay <- Overlay.loadLazy overlayPath
  let Right (pkgDesc0, flags) =
        finalizePackageDescription
          [ -- XXX: common things we should enable/disable?
            -- (FlagName "small_base", True) -- try to use small base
            (FlagName "cocoa", False)
          ]
          (\dep -> trace ("accepting dep(?): " ++ display dep) True)
          -- (Nothing :: Maybe (Index.PackageIndex PackageIdentifier))
          buildPlatform
          (fst GHCCore.defaultGHC)
          [] pkgGenericDesc

      mminimumGHC = GHCCore.minimumGHCVersionToBuildPackage pkgGenericDesc
      (compilerId, excludePkgs) = maybe GHCCore.defaultGHC id mminimumGHC

      pkgDesc = let deps = [ Dependency pn (Cabal.simplifyVersionRange vr)
                           | Dependency pn vr <- buildDepends pkgDesc0
                           , pn `notElem` excludePkgs
                           ]
                in pkgDesc0 { buildDepends = deps }
      edeps = Merge.resolveDependencies overlay pkgDesc (Just compilerId)

  debug verbosity ("Selected flags: " ++ show flags)
  info verbosity ("Guessing GHC version: " ++ maybe "could not guess" (display.fst) mminimumGHC)
  forM_ excludePkgs $
      \(PackageName name) -> info verbosity $ "Excluded packages (come with ghc): " ++ name

  let ebuild =   (\e -> e { E.depend        = Merge.dep edeps } )
               . (\e -> e { E.depend_extra  = Merge.dep_e edeps } )
               . (\e -> e { E.rdepend       = Merge.rdep edeps } )
               . (\e -> e { E.rdepend_extra = Merge.rdep_e edeps } )
               $ C2E.cabal2ebuild pkgDesc

  mergeEbuild verbosity overlayPath (Portage.unCategory cat) ebuild
  when fetch $ do
    let cabal_pkgId = packageId pkgDesc
        norm_pkgName = packageName (Portage.normalizeCabalPackageId cabal_pkgId)
    fetchAndDigest
      verbosity
      (overlayPath </> display cat </> display norm_pkgName)

fetchAndDigest :: Verbosity
               -> FilePath -- ^ directory of ebuild
               -> IO ()
fetchAndDigest verbosity ebuildDir =
  withWorkingDirectory ebuildDir $ do
     notice verbosity "Recalculating digests (repoman manifest)..."
     r <- system "repoman manifest"
     when (r /= ExitSuccess) $
         notice verbosity "repoman manifest failed horribly. Do something about it!"
     return ()

withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory newDir action = do
  oldDir <- getCurrentDirectory
  bracket
    (setCurrentDirectory newDir)
    (\_ -> setCurrentDirectory oldDir)
    (\_ -> action)

mergeEbuild :: Verbosity -> FilePath -> String -> E.EBuild -> IO () 
mergeEbuild verbosity target cat ebuild = do 
  let edir = target </> cat </> E.name ebuild
      elocal = E.name ebuild ++"-"++ E.version ebuild <.> "ebuild"
      epath = edir </> elocal
      emeta = "metadata.xml"
      mpath = edir </> emeta
      default_meta = Portage.makeDefaultMetadata (E.long_desc ebuild)
  createDirectoryIfMissing True edir
  notice verbosity $ "Writing " ++ elocal
  writeFile epath (display ebuild)

  yet_meta <- doesFileExist mpath
  if (not yet_meta) -- TODO: add --force-meta-rewrite to opts
      then do notice verbosity $ "Writing " ++ emeta
              writeFile mpath default_meta
      else do current_meta <- readFile mpath
              when (current_meta /= default_meta) $
                  notice verbosity $ "Default and current " ++ emeta ++ " differ."

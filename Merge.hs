{-# OPTIONS -XPatternGuards #-}
module Merge
  ( merge ) where

import Control.Monad.Error
import Control.Exception
import Data.Maybe
import Data.List
import Distribution.Package
import Distribution.Compiler (CompilerId(..), CompilerFlavor(GHC))
import Distribution.PackageDescription ( PackageDescription(..)
                                       , FlagName(..)
                                       )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Text (display)

import System.Directory ( getCurrentDirectory
                        , setCurrentDirectory
                        , createDirectoryIfMissing
                        )
import System.Cmd (system)
import System.FilePath ((</>))

import qualified Cabal2Ebuild as C2E
import qualified Portage.EBuild as E
import Error as E

import qualified Distribution.Package as Cabal
import qualified Distribution.Version as Cabal

import Distribution.System (buildPlatform)
import Distribution.Verbosity
import Distribution.Simple.Utils

import Network.URI

import Distribution.Client.IndexUtils ( getAvailablePackages )
import Distribution.Client.HttpUtils ( downloadURI )
import qualified Distribution.Client.PackageIndex as Index
import Distribution.Client.Types

import qualified Portage.PackageId as Portage
import qualified Portage.Version as Portage
import qualified Portage.Host as Host
import qualified Portage.Overlay as Overlay
import qualified Portage.Resolve as Portage

import qualified Portage.GHCCore as GHCCore

import qualified Merge.Dependencies as Merge

import Debug.Trace ( trace )

(<->) :: String -> String -> String
a <-> b = a ++ '-':b

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
resolveVersion :: [AvailablePackage] -> Maybe Cabal.Version -> Maybe AvailablePackage
resolveVersion avails Nothing = Just $ maximumBy (comparing packageInfoId) avails
resolveVersion avails (Just ver) = listToMaybe (filter match avails)
  where
    match avail = ver == pkgVersion (packageInfoId avail)

merge :: Verbosity -> Repo -> URI -> [String] -> FilePath -> IO ()
merge verbosity repo serverURI args overlayPath = do
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
  index <- fmap packageIndex $ getAvailablePackages verbosity [ repo ]

  -- find all packages that maches the user specified package name
  availablePkgs <-
    case Index.searchByName index user_pname_str of
      Index.None -> throwEx (PackageNotFound user_pname_str)
      Index.Ambiguous pkgs -> throwEx (ArgumentError ("Ambiguous name: " ++ unwords (map show pkgs)))
      Index.Unambiguous pkg -> return pkg

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
      norm_pkgId = Portage.normalizeCabalPackageId cabal_pkgId
      norm_pkgName = packageName norm_pkgId
  cat <- maybe (Portage.resolveCategory verbosity overlay norm_pkgName) return m_category

  let pkgGenericDesc = packageDescription selectedPkg
      Right (pkgDesc0, flags) =
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
      edeps = Merge.resolveDependencies pkgDesc (Just compilerId)

  debug verbosity ("Selected flags: " ++ show flags)
  info verbosity ("Guessing GHC version: " ++ maybe "could not guess" (display.fst) mminimumGHC)

  let ebuild = fixSrc serverURI (packageId pkgDesc)
               . (\e -> e { E.depend        = Merge.dep edeps } )
               . (\e -> e { E.depend_extra  = Merge.dep_e edeps } )
               . (\e -> e { E.rdepend       = Merge.rdep edeps } )
               . (\e -> e { E.rdepend_extra = Merge.rdep_e edeps } )
               $ C2E.cabal2ebuild pkgDesc

  mergeEbuild verbosity overlayPath (Portage.unCategory cat) ebuild
  fetchAndDigest
    verbosity
    (overlayPath </> display cat </> display norm_pkgName)
    (display cabal_pkgId <.> "tar.gz")
    (mkUri cabal_pkgId)

mkUri :: Cabal.PackageIdentifier -> URI
mkUri pid =
   -- example:
   -- http://hackage.haskell.org/packages/archive/Cabal/1.4.0.2/Cabal-1.4.0.2.tar.gz
   fromJust $ parseURI $
    "http://hackage.haskell.org/packages/archive/"
             </> p_name </> p_ver </> p_name <-> p_ver <.> "tar.gz"
  where
    p_ver = display (packageVersion pid)
    p_name = display (packageName pid)

fetchAndDigest :: Verbosity
               -> FilePath -- ^ directory of ebuild
               -> String -- ^ tarball name
               -> URI -- ^ tarball uri
               -> IO ()
fetchAndDigest verbosity ebuildDir tarballName tarballURI =
  withWorkingDirectory ebuildDir $ do
     repo_info <- Host.getInfo
     let tarDestination = (Host.distfiles_dir repo_info) </> tarballName
     downloadURI verbosity tarballURI tarDestination
     -- Just err -> throwEx (Portage.DownloadFailed (show tarballURI) (show err))
     -- TODO: downloadURI will throw a non-hackport exception if the
     -- download fails
     notice verbosity $ "Saved to " ++ tarDestination
     notice verbosity "Recalculating digests..."
     _ <- system "repoman manifest"
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
  createDirectoryIfMissing True edir
  info verbosity $ "Writing " ++ elocal
  writeFile epath (display ebuild)

fixSrc :: URI -> PackageIdentifier -> E.EBuild -> E.EBuild
fixSrc serverURI p ebuild =
  ebuild {
    E.src_uri = show $ serverURI {
      uriPath =
        uriPath serverURI
          </> display (pkgName p) 
          </> display (pkgVersion p) 
          </> display (pkgName p) ++ "-" ++ display (pkgVersion p) 
          <.> "tar.gz"
      },
    E.homepage = case E.homepage ebuild of
                "" -> "http://hackage.haskell.org/package/"
                        ++ display (pkgName p)
                x -> x
    }

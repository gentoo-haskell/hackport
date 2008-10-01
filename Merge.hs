module Merge where

import Control.Monad.Error
-- import Control.Monad.Error
import Control.Exception
import Data.Char
import Data.Maybe
import Data.List
import Data.Version
import Distribution.Package
import Distribution.Compiler (CompilerId(..), CompilerFlavor(GHC))
import Distribution.PackageDescription ( PackageDescription(..) )
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Text (display)

import System.Directory ( getCurrentDirectory , setCurrentDirectory )
import System.IO
import System.Cmd (system)
import System.FilePath ((</>), splitPath, joinPath, takeFileName)
import qualified Data.Map as Map

import qualified Cabal2Ebuild as E
import Cache
import Error
import GenerateEbuild
import qualified Portage.PackageId as Portage
import Overlays
import P2

import Distribution.System (buildOS, buildArch)
import Distribution.Verbosity
import Distribution.Simple.Utils

import Network.URI
import Network.HTTP

import Cabal2Ebuild

merge :: Verbosity -> URI -> String -> IO ()
merge verbosity serverURI pstr = do
    (m_category, Portage.PN pname, m_version) <- case Portage.parseFriendlyPackage pstr of
      Just v -> return v
      Nothing -> throwEx (ArgumentError ("Could not parse [category/]package[-version]: " ++ show pstr))
    overlayPath <- getOverlayPath verbosity
    overlay <- readPortageTree overlayPath
    cache <- readCache verbosity overlayPath serverURI
    let (indexTree,clashes) = indexToPortage cache overlay
    mapM_ putStrLn clashes
    info verbosity $ "Searching for: "++ pstr
    let pkgs =
          Map.elems
            . Map.filterWithKey (\(P _ pname') _ -> map toLower pname' == map toLower pname)
            $ indexTree
    return ()
    pkg <- case pkgs of
        [] -> throwEx (PackageNotFound pname)
        [xs] -> case m_version of
            Nothing -> return (maximum xs) -- highest version
            Just v -> do
                let ebuilds = filter (\e -> eVersion e == v) xs
                case ebuilds of
                    [] -> throwEx (PackageNotFound (pname ++ '-':show v))
                    [e] -> return e
                    _ -> fail "the impossible happened"
        _ -> fail "the impossible happened"
    category <- do
        case m_category of
            Just (Portage.Category cat) -> return cat
            Nothing -> do
                case pCategory (ePackage pkg) of
                    "hackage" -> return "dev-haskell"
                    c -> return c
    let Just genericDesc = ePkgDesc pkg
        Right (desc, _) = finalizePackageDescription []
                            (Nothing :: Maybe (PackageIndex PackageIdentifier))
                            buildOS buildArch
                            (CompilerId GHC (Version [6,8,2] []))
                            [] genericDesc
    let ebuild = fixSrc serverURI (packageId desc) (E.cabal2ebuild desc)
        ebuildName = category ++ '/': pname ++ "-" ++ display (pkgVersion (packageId desc))
    putStrLn $ "Merging " ++ ebuildName
    putStrLn $ "Destination: " ++ overlayPath
    mergeEbuild overlayPath category ebuild
    let package_name = pkgName (package desc)
        package_version = showVersion (pkgVersion (package desc))
    print genericDesc
    let
      a <-> b = a ++ '-':b
      a <.> b = a ++ '.':b
      url = "http://hackage.haskell.org/packages/archive/"
             </> package_name </> package_version </> package_name <-> package_version <.> "tar.gz"
      Just uri = parseURI url
      tarballName = package_name <-> package_version <.> "tar.gz"
        -- example:
        -- http://hackage.haskell.org/packages/archive/Cabal/1.4.0.2/Cabal-1.4.0.2.tar.gz
    fetchAndDigest
      verbosity
      (overlayPath </> category </> pname)
      tarballName
      uri

fetchAndDigest :: Verbosity
               -> FilePath -- ^ directory of ebuild
               -> String -- ^ tarball name
               -> URI -- ^ tarball uri
               -> IO () 
fetchAndDigest verbosity ebuildDir tarballName tarballURI = do
  withWorkingDirectory ebuildDir $ do
    notice verbosity $ "Fetching " ++ show tarballURI ++ " ..."
    response <- simpleHTTP (Request tarballURI GET [] "")
    case response of
      Left err -> print err
      Right response -> do
        let tarDestination = "/usr/portage/distfiles" </> tarballName
        notice verbosity $ "Writing to " ++ tarDestination
        writeFile tarDestination (rspBody response)
        system "repoman manifest"
        return ()

withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory newDir action = do
  oldDir <- getCurrentDirectory
  bracket
    (setCurrentDirectory newDir)
    (\_ -> setCurrentDirectory oldDir)
    (\_ -> action)

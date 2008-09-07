module Merge where

import Control.Monad.Error
import Data.Char
import Data.Maybe
import Data.List
import Data.Version
import Distribution.Package
import Distribution.Compiler (CompilerId(..), CompilerFlavor(GHC))
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription )
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Text (display)

import System.IO
import Distribution.System (buildOS, buildArch)
import qualified Data.Map as Map

import qualified Cabal2Ebuild as E
import Cache
import Error
import GenerateEbuild
import qualified Portage.PackageId as Portage
import Overlays
import P2

import Distribution.Verbosity
import Distribution.Simple.Utils

import Network.URI

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
    liftIO $ do
        putStrLn $ "Merging " ++ category ++ '/': pname ++ "-" ++ display (pkgVersion (packageId desc))
        putStrLn $ "Destination: " ++ overlayPath
        mergeEbuild overlayPath category ebuild

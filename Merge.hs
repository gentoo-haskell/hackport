module Merge where

import Text.ParserCombinators.Parsec

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
import Package
import Overlays
import P2

import Distribution.Verbosity
import Distribution.Simple.Utils

import Network.URI

import Cabal2Ebuild

merge :: Verbosity -> URI -> String -> IO ()
merge verbose serverURI pstr = do
    (m_category, pname, m_version) <- case parsePVC of
      Right v -> return v
      Left err -> throwEx (ArgumentError ("Could not parse [category/]package[-version]: " ++ show err))
    portdir <- liftIO (getOverlayPath verbose)
    overlay <- liftIO (readPortageTree portdir)
    cache <- liftIO $ readCache portdir
    let (indexTree,clashes) = indexToPortage cache overlay
    mapM_ putStrLn clashes
    info verbose $ "Searching for: "++ pstr
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
            Just cat -> return cat
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
        putStrLn $ "Destination: " ++ portdir
        mergeEbuild portdir category ebuild
    where
    parsePVC = parse readPVC "" pstr
    readPVC = do
        mc <- option Nothing $ try $ do
            c <- readCat
            char '/'
            return (Just c)
        (p, mv) <- readPkgAndVer
        eof
        return (mc, p, mv)

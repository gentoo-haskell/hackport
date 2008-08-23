module Main where

import Control.Monad.Error
import Data.Char
import Data.Maybe
import Data.List
import Data.Version
import Distribution.Package
import Distribution.Compiler (CompilerId(..), CompilerFlavor(GHC))
import Distribution.PackageDescription.Configuration
         ( finalizePackageDescription, flattenPackageDescription )
import Distribution.Simple.PackageIndex (PackageIndex)
import Distribution.Text (display)
import System.IO
import Distribution.System (buildOS, buildArch)
import qualified Data.Map as Map
import Text.ParserCombinators.Parsec

import Action
import qualified Cabal2Ebuild as E
import Cache
import Config
import Diff
import Error
import GenerateEbuild
import Index
import Status
import Package
import Overlays
import P2

import qualified Distribution.PackageDescription as Cabal
import Distribution.Verbosity (normal)
import Cabal2Ebuild

list :: String -> HPAction ()
list name = do
    index <- readCache =<< getOverlayPath
    let index' | null name = index
               | otherwise = filterIndexByPV matchSubstringCaseInsensitive index
        pkgs = [ pkg ++ "-" ++ ver | (pkg,ver,_) <- index']
    if null pkgs
      then throwError (PackageNotFound name)
      else liftIO . putStr . unlines . sort $ pkgs
    where
        matchSubstringCaseInsensitive pName _pVver =
                map toLower name `isInfixOf` map toLower pName

merge :: String -> HPAction ()
merge pstr = do
    (m_category, pname, m_version) <- case parsePVC of
        Right v -> return v
        Left err -> throwError (ArgumentError ("Could not parse [category/]package[-version]: " ++ show err))
    portdir <- getOverlayPath
    overlay <- liftIO $ readPortageTree portdir
    cache <- readCache portdir
    let (indexTree,clashes) = indexToPortage cache overlay
    mapM_ (liftIO . putStrLn) clashes
    whisper $ "Searching for: "++ pstr
    let pkgs =
          Map.elems
            . Map.filterWithKey (\(P _ pname') _ -> map toLower pname' == map toLower pname)
            $ indexTree
    return ()
    pkg <- case pkgs of
        [] -> throwError (PackageNotFound pname)
        [xs] -> case m_version of
            Nothing -> return (maximum xs) -- highest version
            Just v -> do
                let ebuilds = filter (\e -> eVersion e == v) xs
                case ebuilds of
                    [] -> throwError (PackageNotFound (pname ++ '-':show v))
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
    ebuild <- fixSrc (packageId desc) (E.cabal2ebuild desc)
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

makeEbuild :: String -> HPAction ()
makeEbuild cabalFileName = liftIO $ do
    pkg <- Cabal.readPackageDescription normal cabalFileName
    let ebuild = cabal2ebuild (flattenPackageDescription pkg)
    let ebuildFileName = name ebuild ++ "-" ++ version ebuild ++ ".ebuild"
    writeFile ebuildFileName (showEBuild ebuild)

hpmain :: HPAction ()
hpmain = do
    mode <- loadConfig
    requestedUpdate <- fmap refreshCache getCfg
    when requestedUpdate $
        case mode of
            Update -> return ()
            _ -> updateCache
    case mode of
        ShowHelp -> liftIO hackageUsage
        List pkg -> list pkg
        Merge pkg -> merge pkg
        DiffTree dtmode -> diffAction dtmode
        Update -> updateCache
        Status action -> statusAction action
        MakeEbuild cabalFileName -> makeEbuild cabalFileName

main :: IO ()
main = do
    res <- performHPAction hpmain
    case res of
        Right _ -> return ()
        Left err -> do
            hPutStrLn stderr "An error occurred. To get more info run with --verbosity=debug"
            hPutStrLn stderr (hackPortShowError err)

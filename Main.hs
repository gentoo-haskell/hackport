module Main where

import Control.Monad.Error
import Data.Char
import Data.List
import Data.Version
import Distribution.Package
import Distribution.PackageDescription
import System.IO
import qualified Data.Map as Map


import Action
import qualified Cabal2Ebuild as E
import Cache
import Config
import Diff
import Error
import GenerateEbuild
import Index
import OverlayPortageDiff
import Portage
import P2

list :: String -> HPAction ()
list name = do
    cache <- readCache =<< getOverlayPath
    let pkgs | null name = [ pkg | (_,_,pkg) <- cache ]
             | otherwise = searchIndex matchSubstringCaseInsensitive cache
          where matchSubstringCaseInsensitive str _ver =
                  lcaseName `isInfixOf` lcase str
                lcaseName = lcase name
                lcase = map toLower
    if null pkgs
      then throwError (PackageNotFound (Left name))
      else liftIO . putStr . unlines
         . map showPackageId
         . sort
         $ map package pkgs

merge :: PackageIdentifier -> HPAction ()
merge pid = do
    portdir <- getOverlayPath
    overlay <- liftIO $ readPortageTree portdir
    cache <- readCache portdir
    let (indexTree,clashes) = indexToPortage cache overlay
    mapM_ (liftIO . putStrLn) clashes
    whisper $ "Searching for: "++pkgName pid++"-"++showVersion (pkgVersion pid)
    let pkgs = searchIndex (\name vers -> map toLower name == map toLower (pkgName pid) && vers == showVersion (pkgVersion pid)) cache
    case pkgs of
        [] -> throwError (PackageNotFound (Right pid))
        [pkg] -> do
            let categories = [ c | P c p <- (Map.keys indexTree), p == pkgName pid]
            category <- do
                dpc <- fmap defaultPortageCategory getCfg
                case categories of
                    ["hackage"] -> return dpc
                    [c] -> return c
            ebuild <- fixSrc pid (E.cabal2ebuild pkg)
            liftIO $ do
                putStrLn $ "Merging " ++ category ++ '/': pkgName pid ++ '-': showVersion (pkgVersion pid)
                putStrLn $ "Destination: " ++ portdir
                mergeEbuild portdir category ebuild

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
        DiffTree mode -> diffAction mode
        Update -> updateCache
        OverlayOnly -> overlayonly

main :: IO ()
main = performHPAction hpmain

module Main where

import Control.Monad.Error
import Data.Char
import Data.Maybe
import Data.List
import Data.Version
import Distribution.Package
import Distribution.PackageDescription
import System.IO
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
import Portage
import P2
import Utils

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
      then throwError (PackageNotFound name)
      else liftIO . putStr . unlines
         . map showPackageId
         . sort
         $ map package pkgs

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
    category <- do
        case m_category of
            Just cat -> return cat
            Nothing -> do
                case pCategory (ePackage pkg) of
                    "hackage" -> return "dev-haskell"
                    c -> return c
    let desc = fromJust $ ePkgDesc pkg
    ebuild <- fixSrc (package desc) (E.cabal2ebuild desc)
    liftIO $ do
        putStrLn $ "Merging " ++ category ++ '/': pname ++ showPackageId (package desc)
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
        Status -> status

main :: IO ()
main = performHPAction hpmain

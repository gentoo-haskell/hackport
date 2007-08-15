module P2 where

-- Module that respect categories.
-- Possibly to replace Portage.hs when the rest of the project has been
-- ported to this style.

import BlingBling

import Control.Arrow
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.List as List

import System.Directory
import System.IO
import System.IO.Unsafe
import System.FilePath

import Text.Regex

import Version

type Portage = PortageMap [Ebuild]
type PortageMap a = Map Package a

data Ebuild = Ebuild {
    ePackage :: Package,
    eVersion :: Version,
    eFilePath :: FilePath }
        deriving (Eq, Show)

data Package = P String String
    deriving (Eq, Ord)

instance Show Package where
    show (P c p) = c ++ '/':p

lookupEbuildWith :: Portage -> Package -> (Ebuild -> Bool) -> Maybe Ebuild
lookupEbuildWith portage package comp = do
    es <- Map.lookup package portage
    List.find comp es

getPackageList :: FilePath -> IO [Package]
getPackageList portdir = do
    categories <- getDirectories portdir
    packages <- fmap concat $ forMbling categories $ \c -> do
        putStr "."
        pkg <- getDirectories (portdir </> c)
        return (map (P c) pkg)
    putStrLn ""
    return packages

readPortagePackages :: FilePath -> [Package] -> IO (Map Package [Ebuild])
readPortagePackages portdir packages0 = do
    packages <- filterM (doesDirectoryExist . (portdir </>) . show) packages0
    ebuild_map <- forM packages $ \package -> do
        ebuilds <- unsafeInterleaveIO (getPackageVersions package)
        return (package, ebuilds)
    return $ Map.fromList ebuild_map

    where
    getPackageVersions :: Package -> IO [Ebuild]
    getPackageVersions (P category package) = do
        files <- getDirectoryContents (portdir </> category </> package)
        let ebuilds = [ (v, portdir </> category </> package </> fn)
                      | (Just v, fn) <- map ((filterVersion package) &&& id) files ]
        return (map (uncurry (Ebuild (P category package))) ebuilds)

    filterVersion :: String -> String -> Maybe Version
    filterVersion p fn = do
        [vstring] <- matchRegex (ebuildVersionRegex p) fn
        case (parseVersion vstring) of
            Left e -> fail (show e)
            Right v -> return v

    ebuildVersionRegex name = mkRegex ("^"++name++"-(.*)\\.ebuild$")

readPortageTree :: FilePath -> IO (Map Package [Ebuild])
readPortageTree portdir = do
    packages <- getPackageList portdir
    readPortagePackages portdir packages

getDirectories :: FilePath -> IO [String]
getDirectories fp = do
    files <- fmap (filter (`notElem` [".", ".."])) $ getDirectoryContents fp
    filterM (doesDirectoryExist . (fp </>)) files

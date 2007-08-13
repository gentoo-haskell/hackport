module P2 where

import qualified Data.Set as Set

import Control.Arrow
import Control.Monad

import Data.Map (Map)
import qualified Data.Map as Map
import Data.Maybe
import qualified Data.List as List

import System
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


main' = do
    args <- getArgs
    portdir <- case args of
        [] -> return "/usr/portage"
        [x] -> return x
    print =<< (readPortageTree portdir)

readPortageTree :: FilePath -> IO (Map Package [Ebuild])
readPortageTree portdir = do
    categories <- getDirectories portdir
    packages <- fmap concat $ forM categories $ \c -> do
        putStr "."
        pkg <- getDirectories (portdir </> c)
        return (map ((,) c) pkg)
    putStrLn ""
    ebuild_map <- forM packages $ \package -> do
        ebuilds <- unsafeInterleaveIO (getPackageVersions package)
        return (uncurry P package, ebuilds)
    return $ Map.fromList ebuild_map

    where
    getPackageVersions :: (String, String) -> IO [Ebuild]
    getPackageVersions (category, package) = do
        files <- getDirectoryContents (portdir </> category </> package)
        let ebuilds = [ (v, portdir </> category </> package </> fn) | (Just v, fn) <- map ((filterVersion package) &&& id) files ]
        return (map (uncurry (Ebuild (P category package))) ebuilds)

    filterVersion :: String -> String -> Maybe Version
    filterVersion p fn = do
        [vstring] <- matchRegex (ebuildVersionRegex p) fn
        case (parseVersion vstring) of
            Left e -> fail (show e)
            Right v -> return v

    ebuildVersionRegex name = mkRegex ("^"++name++"-(.*)\\.ebuild$")

getDirectories :: FilePath -> IO [String]
getDirectories fp = do
    files <- fmap (filter (`notElem` [".", ".."])) $ getDirectoryContents fp
    filterM (doesDirectoryExist . (fp </>)) files

module Portage where

import System.Directory
import Text.Regex
import Data.Maybe

ebuildVersionRegex :: String -> Regex
ebuildVersionRegex name = mkRegex ("^"++name++"-(.*)\\.ebuild$")

filterPackages :: String -> [String] -> IO [String]
filterPackages _ [] = return []
filterPackages base (x:xs) = do
    ak <- case x of
        "." -> return Nothing
        ".." -> return Nothing
        dir -> do
            exists <- doesDirectoryExist (base++dir)
            return (if exists then Just dir else Nothing)
    rest <- filterPackages base xs
    return (maybe rest (:rest) ak)

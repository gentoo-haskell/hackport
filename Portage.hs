module Portage where

import System.Directory
import Text.Regex
import Data.Maybe

import Bash
import Action
import Config

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

getOverlayPath :: HPAction String
getOverlayPath = do
    cfg <- getCfg
    case overlayPath cfg of
        Nothing -> do
          tree <- getOverlay `sayDebug` ("Guessing overlay from /etc/make.conf...\n",\tree->"Found '"++tree++"'")
          setOverlayPath $ Just tree
          return tree
        Just tree -> return tree


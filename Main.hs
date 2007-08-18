module Main where

import Distribution.Package
import Distribution.PackageDescription
import Data.Version
import Control.Monad.Error
import System.IO
import Data.Char
import Data.List


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
	portTree <- getOverlayPath
	cache <- readCache portTree
	whisper $ "Searching for: "++pkgName pid++"-"++showVersion (pkgVersion pid)
	let pkgs = searchIndex (\name vers -> map toLower name == map toLower (pkgName pid) && vers == showVersion (pkgVersion pid)) cache
	case pkgs of
		[] -> throwError (PackageNotFound (Right pid))
		[pkg] -> do
			ebuild <- fixSrc pid (E.cabal2ebuild pkg)
			mergeEbuild portTree ebuild

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

{-instance Ord PackageIdentifier where
	compare pkg1 pkg2 = case compare (pkgName pkg1) (pkgName pkg2) of
		EQ -> compare (pkgVersion pkg1) (pkgVersion pkg2)
		x -> x-}

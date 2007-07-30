module Main where

import System.Environment
import System.Exit
import Distribution.Package
import Distribution.PackageDescription
import Data.Version
import Control.Monad.Trans
import Control.Monad.Error
import Data.Typeable
import System.IO
import Data.Char
import Data.List
import qualified Data.Set as Set
import qualified Data.Map as Map

import Action
import Error
import Cabal2Ebuild
import GenerateEbuild
import Bash
import Config
import Diff
import Portage
import Cache
import Index
import MaybeRead

list :: String -> HPAction ()
list name = do
	cache <- readCache =<< getPortageTree
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
	portTree <- getPortageTree
	cache <- readCache portTree
	whisper $ "Searching for: "++pkgName pid++"-"++showVersion (pkgVersion pid)
	let pkgs = searchIndex (\name vers -> name == pkgName pid && vers == showVersion (pkgVersion pid)) cache
	case pkgs of
		[] -> throwError (PackageNotFound (Right pid))
		[pkg] -> do 
			ebuild <- fixSrc pid (cabal2ebuild pkg)
			mergeEbuild portTree ebuild

tabs :: String -> String
tabs str = let len = length str in str++(if len < 3*8
	then replicate (3*8-len) ' '
	else "")

showDiffState :: String -> DiffState Version -> String
showDiffState name st = (tabs name) ++ " [" ++ (case st of
	Both x y -> showVersion x ++ (case compare x y of
		EQ -> "="
		GT -> ">"
		LT -> "<") ++ showVersion y
	OnlyLeft x -> showVersion x ++ ">none"
	OnlyRight y ->  "none<"++showVersion y)++"]"

diff :: DiffMode -> HPAction ()
diff mode = do
	cfg <- getCfg
	portTree <- getPortageTree
	cache <- readCache portTree
	--let serverPkgs = map (\(_,_,pd)-> (package pd) {pkgName=map toLower (pkgName $ package pd)}) cache
	let serverPkgs = Map.mapKeys (map toLower) $ bestVersions $ indexMapFromList $ indexToPackageIdentifier cache
	portTree <- getPortageTree
	portagePkgs' <- portageGetPackages portTree
	let portagePkgs = bestVersions $ indexMapFromList portagePkgs'
	let diff = diffBest serverPkgs portagePkgs
	let showFilter st = case mode of
		ShowAll -> True
		ShowMissing -> case st of
			OnlyLeft _ -> True
			Both x y -> x > y
			OnlyRight _ -> False
		ShowAdditions -> case st of
			OnlyLeft _ -> False
			Both x y -> x < y
			OnlyRight _ -> True
		ShowCommon -> case st of
			OnlyLeft _ -> False
			Both x y -> x == y
			OnlyRight _ -> False
	mapM_ (\(name,st) -> if showFilter st
		then info $ showDiffState name st
		else return ()) (Map.assocs diff)

update :: HPAction ()
update = do
	updateCache
	return ()

overlayonly :: Maybe String -> HPAction ()
overlayonly pd = do
	cfg <- getCfg
	portdir <- maybe (getPortDir `sayDebug` ("Guessing portage main dir from /etc/make.conf...",\res->"found: "++res++".")) return pd
	overlay <- getPortageTree
	mainlinepkgs <- portageGetPackages portdir
		`sayDebug` ("Getting package list from "++portdir++"...",const "done.")
	overlaypkgs <- portageGetPackages overlay
		`sayDebug` ("Getting package list from "++overlay++"...",const "done.")
	info "These packages are in the overlay but not in the portage tree:"
	let (_,diff,_) = diffSet (Set.fromList mainlinepkgs) (Set.fromList overlaypkgs)
	let vindent = case verbosity cfg of
		Silent -> id
		_ -> indent
	let showPkgSet set = mapM_ (\pkg->echoLn (pkgName pkg++"-"++showVersion (pkgVersion pkg))) (Set.elems set)
	vindent $ showPkgSet diff
	
hpmain :: HPAction ()
hpmain = do
	mode <- loadConfig
	case mode of
		ShowHelp -> liftIO hackageUsage
		List pkg -> list pkg
		Merge pkg -> merge pkg
		DiffTree mode -> diff mode
		Update -> update
		OverlayOnly portdir -> overlayonly portdir

main :: IO ()
main = performHPAction hpmain

{-instance Ord PackageIdentifier where
	compare pkg1 pkg2 = case compare (pkgName pkg1) (pkgName pkg2) of
		EQ -> compare (pkgVersion pkg1) (pkgVersion pkg2)
		x -> x-}

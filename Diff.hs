module Diff where

import Data.Set as Set (Set)
import qualified Data.Set as Set
import qualified Data.Map as Map

import Control.Monad.Trans
import Data.Char

import Action
import Cache
import Config
import P2
import Portage
import Version

diffSet :: (Eq a,Ord a) 
	=> Set a		-- ^ Set 1
	-> Set a 		-- ^ Set 2
	-> (Set a,Set a,Set a)	-- ^ (Things in 1 but not in 2,Things in 2 but not in 1,Things in both sets)
diffSet set1 set2 = let
	int = Set.intersection set1 set2
	in1 = Set.difference set1 int
	in2 = Set.difference set2 int in (in1,in2,int)

data DiffState a
	= OnlyLeft a
	| OnlyRight a
	| Both a a

tabs :: String -> String
tabs str = let len = length str in str++(if len < 3*8
	then replicate (3*8-len) ' '
	else "")

showDiffState :: Package -> DiffState Version -> String
showDiffState pkg st = (tabs (show pkg)) ++ " [" ++ (case st of
	Both x y -> showVersion x ++ (case compare x y of
		EQ -> "="
		GT -> ">"
		LT -> "<") ++ showVersion y
	OnlyLeft x -> showVersion x ++ ">none"
	OnlyRight y ->  "none<"++showVersion y)++"]"


diffAction :: DiffMode -> HPAction ()
diffAction dm = do
    overlayPath <- getOverlayPath
    cache <- readCache overlayPath
    overlayTree <- liftIO $ readPortageTree overlayPath
    let (hackageTree, clashes) = indexToPortage cache overlayTree
    liftIO $ mapM_ putStrLn clashes
    diff hackageTree overlayTree dm

diff :: Portage -> Portage -> DiffMode -> HPAction ()
diff pt1 pt2 mode = do
        let pkgs1 = Map.map (OnlyLeft  . eVersion . maximum) pt1
        let pkgs2 = Map.map (OnlyRight . eVersion . maximum) pt2
        let union = Map.unionWith (\(OnlyLeft x) (OnlyRight y) -> Both x y) pkgs1 pkgs2
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
		ShowNewer -> case st of
			OnlyLeft _ -> False
			Both x y -> x > y
			OnlyRight _ -> False
		ShowCommon -> case st of
			OnlyLeft _ -> False
			Both x y -> x == y
			OnlyRight _ -> False
        let packages = filter (showFilter . snd) (Map.assocs union)
        mapM_ (info . uncurry showDiffState) packages

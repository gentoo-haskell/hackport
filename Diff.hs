module Diff
    ( runDiff
    , DiffMode(..)
    ) where

import Data.Char
import qualified Data.Map as Map
import Network.URI

import Cache
import P2
import qualified Portage.Version as Portage

-- cabal
import Distribution.Verbosity
import Distribution.Text(display)

data DiffMode
	= ShowAll
	| ShowMissing
	| ShowAdditions
	| ShowNewer
	| ShowCommon
	deriving Eq

data DiffState a
	= OnlyLeft a
	| OnlyRight a
	| Both a a

tabs :: String -> String
tabs str = let len = length str in str++(if len < 3*8
	then replicate (3*8-len) ' '
	else "")

showDiffState :: Package -> DiffState Portage.Version -> String
showDiffState pkg st = (tabs (show pkg)) ++ " [" ++ (case st of
  Both x y -> display x ++ (case compare x y of
    EQ -> "="
    GT -> ">"
    LT -> "<") ++ display y
  OnlyLeft x -> display x ++ ">none"
  OnlyRight y -> "none<" ++ display y) ++ "]"

runDiff :: Verbosity -> FilePath -> URI -> DiffMode -> IO ()
runDiff verbosity overlayPath serverURI dm = do
  cache <- readCache verbosity overlayPath serverURI
  overlayTree <- readPortageTree overlayPath
  let (hackageTree, clashes) = indexToPortage cache overlayTree
  mapM_ putStrLn clashes
  diff hackageTree overlayTree dm

diff :: Portage -> Portage -> DiffMode -> IO ()
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
  mapM_ (putStrLn . uncurry showDiffState) packages

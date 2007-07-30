module Diff where

import Data.Set as Set
import Data.Map as Map

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

diffBest :: (Ord k,Eq a)
	=> Map k a
	-> Map k a
	-> Map k (DiffState a)
diffBest map1 map2 = Map.unionWith mergeDiffState
	(Map.map OnlyLeft map1)
	(Map.map OnlyRight map2)
	where
	mergeDiffState :: DiffState a -> DiffState a -> DiffState a
	mergeDiffState (OnlyLeft x) (OnlyRight y) = Both x y

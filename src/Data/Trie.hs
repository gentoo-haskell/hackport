{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveTraversable #-}
{-# LANGUAGE Strict #-}

module Data.Trie
    ( Trie(..)
    , singleton
    , lookup
    , toList
    ) where

import Control.Applicative
import Control.Monad
import Data.Binary
import qualified Data.DList as DList
import Data.DList (DList)
import qualified Data.Foldable as F
import qualified Data.Map.Strict as Map
import Data.Map.Strict (Map)
import GHC.Generics

import Prelude hiding (lookup)

-- | A tree with branches keyed by a 'Map', which is useful for storing
--   and searching list prefixes in a compact way. Each node is marked
--   with a type signifying list enedings.
--
--   Example: @"he"@ and @"hi"@ stored in a @'Trie' 'Char' '()'@ would look like:
--
--   @Nothing :< toList [('h', Nothing :< toList [('e', Just () :< toList []),('i', Just () :< toList [])])]@
data Trie k a = Maybe a :< Map k (Trie k a)
    deriving (Show, Eq, Ord, Functor, Foldable, Traversable, Generic)

instance Ord k => Semigroup (Trie k a) where
    ma1 :< f1 <> ma2 :< f2 = (ma2 <|> ma1) :< Map.unionWith (<>) f1 f2

instance Ord k => Monoid (Trie k a) where
    mempty = Nothing :< Map.empty

instance (Binary k, Binary a) => Binary (Trie k a)

-- | Walk the 'Trie' using the given key list looking for a matching node.
lookup :: Ord k => Trie k a -> [k] -> Maybe (Trie k a)
lookup (_ :< mapK) (k:ks) =
    Map.lookup k mapK >>= (\c -> lookup c ks)
lookup t [] = Just t

-- | Return the path and value of every node marked with 'Just'
toList :: Trie k a -> [([k],a)]
toList = DList.toList . go DList.empty DList.empty
  where
    go :: DList ([k], a) -> DList k -> Trie k a -> DList ([k], a)
    go justs path (b :< m) =
        let justs' = case b of
                Just x -> justs `DList.snoc` (DList.toList path, x)
                Nothing -> justs
        in justs' <> F.foldMap'
            (\(x,t) -> go justs' (path `DList.snoc` x) t)
            (Map.toAscList m)

-- | Create a simple 'Trie' from a key list and a value.
singleton :: [k] -> a -> Trie k a
singleton l x = go l
  where
    go (k:ks) = Nothing :< Map.singleton k (go ks)
    go []     = Just x :< Map.empty

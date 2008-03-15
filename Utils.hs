module Utils where

comparing :: (Eq b) => (a -> b) -> a -> a -> Bool
comparing f x y = f x == f y

compareWith :: (Ord b) => (a -> b) -> a -> a -> Ordering
compareWith f x y = compare (f x) (f y)

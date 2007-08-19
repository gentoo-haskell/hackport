module Utils where

comparing f x y = f x == f y

compareWith f x y = compare (f x) (f y)

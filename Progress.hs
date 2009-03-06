-----------------------------------------------------------------------------
-- |
-- Module      :  Progress
-- Copyright   :  (c) Duncan Coutts 2008
-- License     :  BSD-like
--
-- Maintainer  :  duncan@haskell.org
-- Portability :  portable
--
-- Common types for dependency resolution.
-----------------------------------------------------------------------------
module Progress (
    Progress(..),
    fold, unfold, fromList,
  ) where

import Prelude hiding (fail)

-- | A type to represent the unfolding of an expensive long running
-- calculation that may fail. We may get intermediate steps before the final
-- retult which may be used to indicate progress and\/or logging messages.
--
data Progress step fail done = Step step (Progress step fail done)
                             | Fail fail
                             | Done done

-- | Consume a 'Progres' calculation. Much like 'foldr' for lists but with
-- two base cases, one for a final result and one for failure.
--
-- Eg to convert into a simple 'Either' result use:
--
-- > foldProgress (flip const) Left Right
--
fold :: (step -> a -> a) -> (fail -> a) -> (done -> a)
     -> Progress step fail done -> a
fold step fail done = go
  where
    go (Step s p) = step s (go p)
    go (Fail f)   = fail f
    go (Done r)   = done r

unfold :: (s -> Either (Either fail done) (step, s))
       -> s -> Progress step fail done
unfold f = go
  where
    go s = case f s of
      Left (Left  fail) -> Fail fail
      Left (Right done) -> Done done
      Right (step, s')  -> Step step (go s')

fromList :: [a] -> Progress () b [a]
fromList xs0 = unfold next xs0
  where
    next []     = Left (Right xs0)
    next (_:xs) = Right ((), xs)

instance Functor (Progress step fail) where
  fmap f = fold Step Fail (Done . f)

instance Monad (Progress step fail) where
  return a = Done a
  p >>= f  = fold Step Fail f p

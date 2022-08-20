{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE FlexibleContexts #-}

module Hackport.Env
  (
    -- * Env monad
    MonadEnv
  , Env
  , runEnv
    -- * Global env
  , GlobalEnv (..)
    -- * Subcommand env
    -- ** List
  , ListEnv (..)
    -- ** MakeEbuildEnv
  , MakeEbuildEnv (..)
    -- ** StatusEnv
  , StatusEnv (..)
    -- ** MergeEnv
  , MergeEnv (..)
    -- * Re-exports
  , module Control.Monad.Reader
  ) where

import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Last(..))
import qualified Distribution.Verbosity as V

import Status (StatusDirection)

type MonadEnv env m = (MonadReader (GlobalEnv, env) m, MonadIO m)
type Env env = ReaderT (GlobalEnv, env) IO

data GlobalEnv = GlobalEnv
  { globalVerbosity :: V.Verbosity
  , globalPathToOverlay :: Maybe FilePath
  , globalPathToPortage :: Maybe FilePath
  }
  deriving (Show, Eq, Ord)

instance Semigroup GlobalEnv where
  GlobalEnv v1 po1 pp1 <> GlobalEnv v2 po2 pp2 =
    GlobalEnv
      (getLast (Last v1 <> Last v2))
      (getLast <$> ((Last <$> po1) <> (Last <$> po2)))
      (getLast <$> ((Last <$> pp1) <> (Last <$> pp2)))

instance Monoid GlobalEnv where
  mempty = GlobalEnv V.normal Nothing Nothing

newtype ListEnv = ListEnv
  { listPackages :: [String]
  }
  deriving (Show, Eq, Ord)

data MakeEbuildEnv = MakeEbuildEnv
  { makeEbuildCategory :: String
  , makeEbuildCabalFiles :: NonEmpty FilePath
  , makeEbuildCabalFlags :: Maybe String
  }
  deriving (Show, Eq, Ord)

data StatusEnv = StatusEnv
  { statusDirection :: StatusDirection
  , statusPackages :: [String]
  }
  deriving (Eq)

data MergeEnv = MergeEnv
  { mergeCabalFlags :: Maybe String
  , mergePackage :: String
  }

runEnv
  :: Env env a
  -> env
  -> GlobalEnv
  -> IO a
runEnv env e global = runReaderT env (global, e)

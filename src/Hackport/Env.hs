{-# LANGUAGE ConstraintKinds #-}
{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

module Hackport.Env
  (
    -- * Env monad
    MonadEnv
  , HasEnv (..)
  , HasGlobalEnv (..)
  , Env
  , runEnv
    -- * Global env
  , GlobalEnv (..)
    -- * Subcommand env
    -- ** List
  , ListEnv (..)
    -- ** Make Ebuild
  , MakeEbuildEnv (..)
    -- ** Status
  , StatusEnv (..)
    -- ** Merge
  , MergeEnv (..)
    -- * Metadata
  , WritesMetadata (..)
  , UseHackageRemote
    -- * Re-exports
  , module Control.Monad.Reader
  ) where

import Control.Monad.Reader
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Last(..))
import qualified Distribution.Verbosity as V

import Status.Types (StatusDirection)

type MonadEnv env m = (HasGlobalEnv m, HasEnv env m, MonadIO m)
type Env env = ReaderT (GlobalEnv, env) IO

class HasGlobalEnv m where
  askGlobalEnv :: m GlobalEnv

instance Monad m => HasGlobalEnv (ReaderT (GlobalEnv,env) m) where
  askGlobalEnv = asks fst

class HasEnv env m where
  askEnv :: m env

instance Monad m => HasEnv env (ReaderT (a,env) m) where
  askEnv = asks snd

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

class WritesMetadata a where
  useHackageRemote :: a -> UseHackageRemote

type UseHackageRemote = Bool

data MakeEbuildEnv = MakeEbuildEnv
  { makeEbuildCategory :: String
  , makeEbuildCabalFiles :: NonEmpty FilePath
  , makeEbuildCabalFlags :: Maybe String
  , makeEbuildUseHackageRemote :: UseHackageRemote
  }
  deriving (Show, Eq, Ord)

instance WritesMetadata MakeEbuildEnv where
  useHackageRemote = makeEbuildUseHackageRemote

data StatusEnv = StatusEnv
  { statusDirection :: StatusDirection
  , statusPackages :: [String]
  }
  deriving (Eq)

data MergeEnv = MergeEnv
  { mergeCabalFlags :: Maybe String
  , mergePackage :: String
  }

instance WritesMetadata MergeEnv where
  useHackageRemote _ = True

runEnv
  :: Env env a
  -> env
  -> GlobalEnv
  -> IO a
runEnv env e global = runReaderT env (global, e)

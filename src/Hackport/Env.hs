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
  , WarningBuffer
  , getWarningBuffer
  , modifyWarningBuffer
  , displayWarnings
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
import Control.Monad.State.Strict
import qualified Data.DList as DL
import qualified Data.List as L
import Data.List.NonEmpty (NonEmpty (..))
import Data.Semigroup (Last(..))
import qualified Distribution.Simple.Utils as Cabal
import qualified Distribution.Verbosity as V

import Status.Types (StatusDirection)

-- | @hackport@ is noisy. Hold off on displaying any warnings until the bulk
--   of the output has been printed. We use a 'DL.DList' to hold the pending
--   warnings.
type WarningBuffer = DL.DList String

type MonadEnv env m =
    ( HasGlobalEnv m
    , HasEnv env m
    , MonadIO m
    , MonadState WarningBuffer m
    )
type Env env = ReaderT (GlobalEnv, env) (StateT WarningBuffer IO)

class HasGlobalEnv m where
  askGlobalEnv :: m GlobalEnv

instance Monad m => HasGlobalEnv (ReaderT (GlobalEnv,env) m) where
  askGlobalEnv = asks fst

class HasEnv env m where
  askEnv :: m env

instance Monad m => HasEnv env (ReaderT (a,env) m) where
  askEnv = asks snd

getWarningBuffer :: MonadState WarningBuffer m => m WarningBuffer
getWarningBuffer = get

modifyWarningBuffer :: MonadState WarningBuffer m
  => (WarningBuffer -> WarningBuffer) -> m ()
modifyWarningBuffer = modify

-- | Read the warning buffer and output using 'Cabal.warn'.
displayWarnings :: MonadIO m => V.Verbosity -> WarningBuffer -> m ()
displayWarnings v dl =
    liftIO $ Cabal.warn v $ unlines $ L.intercalate [""] $
        ["hackport emitted the following warnings:"]
            : (indent <$> DL.toList dl)
  where
    indent :: String -> [String]
    indent = map ("  " ++) . lines

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

-- | Run with environment information. This calls 'displayWarnings' at the
--   end of the execution.
runEnv
  :: Env env a
  -> env
  -> GlobalEnv
  -> IO a
runEnv env e global = do
    (a, dl) <- runStateT (runReaderT env (global, e)) DL.empty
    displayWarnings (V.verboseNoWrap (globalVerbosity global)) dl
    pure a

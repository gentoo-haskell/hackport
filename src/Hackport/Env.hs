{-# LANGUAGE DerivingVia #-}
{-# LANGUAGE FlexibleInstances #-}

module Hackport.Env
  (
    -- * Env monad
    Env
  , runEnv
  , HasGlobalEnv(..)
  , HasEnv (..)
    -- ** Warning buffer
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
  , module Control.Monad
  , module Control.Monad.Reader
  ) where

import Control.Monad
import Control.Monad.Reader
import Control.Monad.State.Strict (StateT, MonadState, runStateT, get, modify)
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

-- | Monad that carries command line information and 'WarningBuffer' state
type Env env = ReaderT (GlobalEnv, env) (StateT WarningBuffer IO)

class HasGlobalEnv m where
  -- | Convenient way to grab the 'GlobalEnv'
  askGlobalEnv :: m GlobalEnv

instance Monad m => HasGlobalEnv (ReaderT (GlobalEnv,env) m) where
  askGlobalEnv = asks fst

class HasEnv env m where
  -- | Convenient way to grab the custom @env@ data
  askEnv :: m env

instance Monad m => HasEnv env (ReaderT (a,env) m) where
  askEnv = asks snd

-- | Convenient way to get the current 'WarningBuffer' state
getWarningBuffer :: MonadState WarningBuffer m => m WarningBuffer
getWarningBuffer = get

-- | Convenient way to modify the current 'WarningBuffer' state
modifyWarningBuffer :: MonadState WarningBuffer m
  => (WarningBuffer -> WarningBuffer) -> m ()
modifyWarningBuffer = modify

-- | Read the warning buffer and output using 'Cabal.warn'.
displayWarnings :: MonadIO m => V.Verbosity -> WarningBuffer -> m ()
displayWarnings v dl = unless (null dl) $ liftIO . Cabal.warn v
  $ unlines
    -- insert an empty line between each paragraph
    $ L.intercalate [""]
      -- A list of strings forms a paragraph
      $ ["hackport emitted the following warnings:"]
        -- Add indentation to each string in the warning buffer
        -- This forms a list of paragraphs (one for each warning)
        : (indent <$> DL.toList dl)
  where
    indent :: String -> [String]
    indent = map ("  " ++) . lines

-- | Environment info that every @hackport@ subcommand needs
data GlobalEnv = GlobalEnv
  { globalVerbosity :: V.Verbosity
  , globalPathToOverlay :: Maybe FilePath
  , globalPathToPortage :: Maybe FilePath
  }
  deriving (Show, Eq, Ord)
  deriving Semigroup via Last GlobalEnv

instance Monoid GlobalEnv where
  mempty = GlobalEnv V.normal Nothing Nothing

-- | Environment info specific to the @list@ subcommand
newtype ListEnv = ListEnv
  { listPackages :: [String]
  }
  deriving (Show, Eq, Ord)

-- | Class for the @make-ebuild@ and @merge@ subcommands, which write to
--   @metadata.xml@
class WritesMetadata a where
  -- | Should @hackport@ write @hackage@ remote info to @metadata.xml@?
  useHackageRemote :: a -> UseHackageRemote

type UseHackageRemote = Bool

-- | Environment info specific to the @make-ebuild@ subcommand
data MakeEbuildEnv = MakeEbuildEnv
  { makeEbuildCategory :: String
  , makeEbuildCabalFiles :: NonEmpty FilePath
  , makeEbuildCabalFlags :: Maybe String
  , makeEbuildUseHackageRemote :: UseHackageRemote
  }
  deriving (Show, Eq, Ord)

instance WritesMetadata MakeEbuildEnv where
  useHackageRemote = makeEbuildUseHackageRemote

-- | Environment info specific to the @status@ subcommand
data StatusEnv = StatusEnv
  { statusDirection :: StatusDirection
  , statusPackages :: [String]
  }
  deriving (Eq)

-- | Environment info specific to the @merge@ subcommand
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

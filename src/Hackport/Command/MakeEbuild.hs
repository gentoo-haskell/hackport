module Hackport.Command.MakeEbuild
  ( makeEbuildAction
  ) where

import qualified Distribution.PackageDescription.Parsec as Cabal
import Distribution.Parsec (simpleParsec)
import qualified Distribution.Verbosity as V

import Error
import Merge
import Overlays

import Hackport.Env

makeEbuildAction :: MonadEnv MakeEbuildEnv m => m ()
makeEbuildAction = do
  (GlobalEnv verbosity op _, MakeEbuildEnv catstr cabals flags) <- ask
  cat <- case simpleParsec catstr of
            Just c -> return c
            Nothing -> throw (ArgumentError ("could not parse category: " ++ catstr))
  overlayPath <- liftIO $ getOverlayPath verbosity op
  forM_ cabals $ \cabalFileName -> do
    pkg <- liftIO $ Cabal.readGenericPackageDescription V.normal cabalFileName
    liftIO $ mergeGenericPackageDescription verbosity overlayPath cat pkg False flags

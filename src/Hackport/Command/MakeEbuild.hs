{-# LANGUAGE CPP #-}

module Hackport.Command.MakeEbuild
  ( makeEbuildAction
  ) where

#if MIN_VERSION_mtl(2,3,0)
import Control.Monad
#endif
import qualified Distribution.Simple.PackageDescription as Cabal
import Distribution.Parsec (simpleParsec)
import qualified Distribution.Verbosity as V

import Error
import Merge

import Hackport.Env

makeEbuildAction :: Env MakeEbuildEnv ()
makeEbuildAction = do
  (_, MakeEbuildEnv catstr cabals flags _) <- ask
  cat <- case simpleParsec catstr of
            Just c -> return c
            Nothing -> throw (ArgumentError ("could not parse category: " ++ catstr))
  forM_ cabals $ \cabalFileName -> do
    pkg <- liftIO $ Cabal.readGenericPackageDescription V.normal cabalFileName
    mergeGenericPackageDescription cat pkg False flags

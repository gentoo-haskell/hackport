{-# LANGUAGE LambdaCase #-}

-- | Support for CABAL_FEATURES="..." in haskell-cabal .ebuild files.
-- See haskell-cabal.eclass for details on each of those.
module Portage.EBuild.CabalFeature (CabalFeature(..)) where

import Portage.EBuild.Render

-- | Type representing @CABAL_FEATURES@ in an ebuild.
data CabalFeature = Lib
                  | Profile
                  | Haddock
                  | Hoogle
                  | HsColour
                  | TestSuite
    deriving (Show, Read, Eq)

instance Render CabalFeature where
    render = \case
                 Lib        -> "lib"
                 Profile    -> "profile"
                 Haddock    -> "haddock"
                 Hoogle     -> "hoogle"
                 HsColour   -> "hscolour"
                 TestSuite  -> "test-suite"

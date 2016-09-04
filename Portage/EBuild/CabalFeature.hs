{-# LANGUAGE LambdaCase #-}

-- CABAL_FEATURES="..." in haskell-cabal .ebuild files
-- see haskell-cabal.eclass for details on each of those
module Portage.EBuild.CabalFeature (CabalFeature(..)) where

import Portage.EBuild.Render

data CabalFeature = Lib
                  | Profile
                  | Haddock
                  | Hoogle
                  | HsColour
                  | TestSuite
    deriving Eq

instance Render CabalFeature where
    render = \case
                 Lib        -> "lib"
                 Profile    -> "profile"
                 Haddock    -> "haddock"
                 Hoogle     -> "hoogle"
                 HsColour   -> "hscolour"
                 TestSuite  -> "test-suite"

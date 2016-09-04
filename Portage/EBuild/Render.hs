-- CABAL_FEATURES="..." in haskell-cabal .ebuild files
module Portage.EBuild.Render (Render(..)) where

class Render a where
    render :: a -> String


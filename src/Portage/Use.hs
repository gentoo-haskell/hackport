{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}

module Portage.Use (
  -- * main structures
  UseFlag(..),
  Use(..),
  dispUses,
  -- * helpers
  mkUse,
  mkNotUse,
  mkQUse
  ) where

-- import qualified Text.PrettyPrint as Disp
-- import Text.PrettyPrint ((<>))
-- import Distribution.Pretty (Pretty(..), prettyShow)
-- import qualified Prettyprinter as PP
import           Prettyprinter

import           Control.DeepSeq (NFData(..))

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

-- | Use variable modificator
data UseFlag = UseFlag Use           -- ^ no modificator
             | E UseFlag             -- ^ = modificator (Equiv    mark)
             | Q UseFlag             -- ^ ? modificator (Question mark)
             | X UseFlag             -- ^ ! modificator (eXclamation mark)
             | N UseFlag             -- ^ - modificator 
             deriving (Eq,Show,Ord,Read)

instance NFData UseFlag where
  rnf (UseFlag u) = rnf u
  rnf (E f) = rnf f
  rnf (Q f) = rnf f
  rnf (X f) = rnf f
  rnf (N f) = rnf f

instance Pretty UseFlag where
  pretty = showModificator

mkUse :: Use -> UseFlag
mkUse  = UseFlag 

mkNotUse :: Use -> UseFlag
mkNotUse = N . UseFlag

mkQUse :: Use -> UseFlag
mkQUse = Q . UseFlag

showModificator :: UseFlag -> Doc ann
showModificator (UseFlag u) = pretty u
showModificator (X u)     = "!" <> pretty u
showModificator (Q u)     = pretty u <> "?"
showModificator (E u)     = pretty u <> "="
showModificator (N u)     = "-" <> pretty u

dispUses :: [UseFlag] -> Doc ann
dispUses [] = emptyDoc
dispUses us = brackets $ hcat $ (punctuate ",") $ map pretty us

newtype Use = Use String
    deriving (Eq, Read, Show)

instance NFData Use where
  rnf (Use s) = rnf s

instance Pretty Use where
  pretty (Use u) = pretty u

instance Ord Use where
    compare (Use a) (Use b) = case (a,b) of
        ("test", "test") -> EQ
        ("test", _)      -> LT
        (_, "test")      -> GT
        (_, _)           -> a `compare` b

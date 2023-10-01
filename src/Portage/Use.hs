{-# LANGUAGE CPP #-}

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

import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))
import Distribution.Pretty (Pretty(..))

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

showModificator :: UseFlag -> Disp.Doc
showModificator (UseFlag u) = pretty u
showModificator (X u)     = Disp.char '!' <> pretty u
showModificator (Q u)     = pretty u <> Disp.char '?'
showModificator (E u)     = pretty u <> Disp.char '='
showModificator (N u)     = Disp.char '-' <> pretty u

dispUses :: [UseFlag] -> Disp.Doc
dispUses [] = Disp.empty
dispUses us = Disp.brackets $ Disp.hcat $ (Disp.punctuate (Disp.text ",")) $ map pretty us

newtype Use = Use String
    deriving (Eq, Read, Show)

instance NFData Use where
  rnf (Use s) = rnf s

instance Pretty Use where
  pretty (Use u) = Disp.text u

instance Ord Use where
    compare (Use a) (Use b) = case (a,b) of
        ("test", "test") -> EQ
        ("test", _)      -> LT
        (_, "test")      -> GT
        (_, _)           -> a `compare` b

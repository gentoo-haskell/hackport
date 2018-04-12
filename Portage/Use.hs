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
import qualified Distribution.Text as DT

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

mkUse :: Use -> UseFlag
mkUse  = UseFlag 

mkNotUse :: Use -> UseFlag
mkNotUse = N . UseFlag

mkQUse :: Use -> UseFlag
mkQUse = Q . UseFlag

instance DT.Text UseFlag where
  disp = showModificator
  parse = error "instance DT.Text UseFlag: not implemented"

showModificator :: UseFlag -> Disp.Doc
showModificator (UseFlag u) = DT.disp u
showModificator (X u)     = Disp.char '!' <> DT.disp u
showModificator (Q u)     = DT.disp u <> Disp.char '?'
showModificator (E u)     = DT.disp u <> Disp.char '='
showModificator (N u)     = Disp.char '-' <> DT.disp u

dispUses :: [UseFlag] -> Disp.Doc
dispUses [] = Disp.empty
dispUses us = Disp.brackets $ Disp.hcat $ (Disp.punctuate (Disp.text ", ")) $ map DT.disp  us

newtype Use = Use String
    deriving (Eq, Read, Show)

instance Ord Use where
    compare (Use a) (Use b) = case (a,b) of
        ("test", "test") -> EQ
        ("test", _)      -> LT
        (_, "test")      -> GT
        (_, _)           -> a `compare` b

instance DT.Text Use where
    disp (Use u) = Disp.text u
    parse = error "instance DT.Text Use: not implemented"

module Portage.Use (
  -- * main structures
  UseFlag(..),
  Use,
  dispUses,
  -- * helpers
  mkUse,
  mkNotUse,
  mkQUse
  ) where

import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))
import Distribution.Text ( Text(..) )

-- | Use variable modificator
data UseFlag = UseFlag Use           -- ^ no modificator
             | E UseFlag             -- ^ = modificator (Equiv    mark)
             | Q UseFlag             -- ^ ? modificator (Question mark)
             | X UseFlag             -- ^ ! modificator (eXclamation mark)
             | N UseFlag             -- ^ - modificator 
             deriving (Eq,Show,Ord,Read)


{-
instance IsString UseFlag where
  fromString ('!':str) = X (fromString str)
  fromString ('-':str) = N (fromString str)
  fromString str       = case last str of
                            '?' -> Q (fromString (init str))
                            '=' -> E (fromString (init str))
                            s   -> UseFlag s
-}
mkUse :: Use -> UseFlag
mkUse  = UseFlag 

mkNotUse :: Use -> UseFlag
mkNotUse = N . UseFlag

mkQUse :: Use -> UseFlag
mkQUse = Q . UseFlag


instance Text UseFlag where
  disp = showModificator

showModificator :: UseFlag -> Disp.Doc
showModificator (UseFlag u) = Disp.text u
showModificator (X u)     = Disp.char '!' <> disp u
showModificator (Q u)     = disp u <> Disp.char '?'
showModificator (E u)     = disp u <> Disp.char '='
showModificator (N u)     = Disp.char '-' <> disp u

dispUses :: [UseFlag] -> Disp.Doc
dispUses [] = Disp.empty
dispUses us = Disp.brackets $ Disp.hcat $ (Disp.punctuate (Disp.text ", ")) $ map disp  us

type Use = String


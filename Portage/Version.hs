{-|
    Author      :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Version parser, according to Portage spec.

    Shamelessly borrowed from exi, ported from Parsec to ReadP

-}

module Portage.Version (
    Version(..),
    Suffix(..),
    fromCabalVersion,
    toCabalVersion,
    is_live
  ) where

import qualified Distribution.Version as Cabal

import Distribution.Text (Text(..))

import qualified Distribution.Compat.ReadP as Parse
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))
import qualified Data.Char as Char (isAlpha, isDigit)

data Version = Version { versionNumber   :: [Int]         -- [1,42,3] ~= 1.42.3
                       , versionChar     :: (Maybe Char)  -- optional letter
                       , versionSuffix   :: [Suffix]
                       , versionRevision :: Int           -- revision, 0 means none
                       }
  deriving (Eq, Ord, Show, Read)

-- foo-9999* is treated as live ebuild
-- Cabal-1.17.9999* as well
is_live :: Version -> Bool
is_live v =
    case vs of
        -- nonempty
        (_:_) | many_nines (last vs) -> True
        _                            -> False
  where vs = versionNumber v
        many_nines n = is_big n && all_nines n
        is_big n     = n >= 9999
        all_nines n  = (all (== '9') . show) n

data Suffix = Alpha Int | Beta Int | Pre Int | RC Int | P Int
  deriving (Eq, Ord, Show, Read)

fromCabalVersion :: Cabal.Version -> Version
fromCabalVersion (Cabal.Version nums _tags) = Version nums Nothing [] 0

toCabalVersion :: Version -> Maybe Cabal.Version
toCabalVersion (Version nums Nothing [] _) = Just (Cabal.Version nums [])
toCabalVersion _                           = Nothing

instance Text Version where
  disp (Version ver c suf rev) =
    dispVer ver <> dispC c <> dispSuf suf <> dispRev rev
    where
      dispVer   = Disp.hcat . Disp.punctuate (Disp.char '.') . map Disp.int
      dispC     = maybe Disp.empty Disp.char
      dispSuf   = Disp.hcat . map disp
      dispRev 0 = Disp.empty
      dispRev n = Disp.text "-r" <> Disp.int n

  parse = do
    ver <- Parse.sepBy1 digits (Parse.char '.')
    c   <- Parse.option Nothing (fmap Just (Parse.satisfy Char.isAlpha))
    suf <- Parse.many parse
    rev <- Parse.option 0 (Parse.string "-r" >> digits)
    return (Version ver c suf rev)

instance Text Suffix where
  disp suf = case suf of
    Alpha n -> Disp.text "_alpha" <> dispPos n
    Beta n  -> Disp.text "_beta"  <> dispPos n
    Pre n   -> Disp.text "_pre"   <> dispPos n
    RC n    -> Disp.text "_rc"    <> dispPos n
    P  n    -> Disp.text "_p"     <> dispPos n

    where
      dispPos :: Int -> Disp.Doc
      dispPos 0 = Disp.empty
      dispPos n = Disp.int n

  parse = Parse.char '_'
       >> Parse.choice
    [ Parse.string "alpha" >> fmap Alpha maybeDigits
    , Parse.string "beta"  >> fmap Beta  maybeDigits
    , Parse.string "pre"   >> fmap Pre   maybeDigits
    , Parse.string "rc"    >> fmap RC    maybeDigits
    , Parse.string "p"     >> fmap P     maybeDigits
    ]
    where
      maybeDigits = Parse.option 0 digits

digits :: Parse.ReadP r Int
digits = fmap read (Parse.munch1 Char.isDigit)

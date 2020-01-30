{-# LANGUAGE CPP #-}

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

import Distribution.Pretty (Pretty(..))

import Distribution.Parsec.Class (Parsec(..))
import qualified Distribution.Compat.CharParsing as P
import qualified Text.PrettyPrint as Disp
import Text.PrettyPrint ((<>))
import qualified Data.Char as Char (isAlpha, isDigit)

#if MIN_VERSION_base(4,11,0)
import Prelude hiding ((<>))
#endif

data Version = Version { versionNumber   :: [Int]         -- [1,42,3] ~= 1.42.3
                       , versionChar     :: (Maybe Char)  -- optional letter
                       , versionSuffix   :: [Suffix]
                       , versionRevision :: Int           -- revision, 0 means none
                       }
  deriving (Eq, Ord, Show, Read)

instance Pretty Version where
  pretty (Version ver c suf rev) =
    dispVer ver <> dispC c <> dispSuf suf <> dispRev rev
    where
      dispVer   = Disp.hcat . Disp.punctuate (Disp.char '.') . map Disp.int
      dispC     = maybe Disp.empty Disp.char
      dispSuf   = Disp.hcat . map pretty
      dispRev 0 = Disp.empty
      dispRev n = Disp.text "-r" <> Disp.int n

instance Parsec Version where
  parsec = do
    ver <- P.sepBy1 digits (P.char '.')
    c   <- P.optional $ P.satisfy Char.isAlpha
    suf <- P.many parsec
    rev <- P.option 0 $ P.string "-r" *> digits
    return $ Version ver c suf rev
  
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

instance Pretty Suffix where
  pretty suf = case suf of
    Alpha n -> Disp.text "_alpha" <> dispPos n
    Beta n  -> Disp.text "_beta"  <> dispPos n
    Pre n   -> Disp.text "_pre"   <> dispPos n
    RC n    -> Disp.text "_rc"    <> dispPos n
    P  n    -> Disp.text "_p"     <> dispPos n

    where
      dispPos :: Int -> Disp.Doc
      dispPos 0 = Disp.empty
      dispPos n = Disp.int n

instance Parsec Suffix where
  parsec = P.char '_'
       >> P.choice
    [ P.string "alpha" >> fmap Alpha maybeDigits
    , P.string "beta"  >> fmap Beta  maybeDigits
    , P.string "pre"   >> fmap Pre   maybeDigits
    , P.string "rc"    >> fmap RC    maybeDigits
    , P.string "p"     >> fmap P     maybeDigits
    ]
    where
      maybeDigits = P.option 0 digits

fromCabalVersion :: Cabal.Version -> Version
fromCabalVersion cabal_version = Version (Cabal.versionNumbers cabal_version) Nothing [] 0

toCabalVersion :: Version -> Maybe Cabal.Version
toCabalVersion (Version nums Nothing [] _) = Just (Cabal.mkVersion nums)
toCabalVersion _                           = Nothing

digits :: P.CharParsing m => m Int
digits = read <$> P.munch1 Char.isDigit

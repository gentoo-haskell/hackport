{-# LANGUAGE CPP #-}
{-# LANGUAGE OverloadedStrings #-}
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

import           Distribution.Parsec (Parsec(..))
import qualified Distribution.Compat.CharParsing as P
import qualified Data.List.NonEmpty as NE
import           Prettyprinter

import           Control.DeepSeq (NFData(..))

-- | Portage-style version type.
data Version = Version { versionNumber   :: [Int]        -- ^ @[1,42,3]@ ~= 1.42.3
                       , versionChar     :: (Maybe Char) -- ^ optional letter
                       , versionSuffix   :: [Suffix]
                       , versionRevision :: Int          -- ^ revision, 0 means none
                       }
  deriving (Eq, Ord, Show, Read)

instance NFData Version where
  rnf (Version n c s r) = rnf n `seq` rnf c `seq` rnf s `seq` rnf r

-- | Prints a valid Portage 'Version' string.
instance Pretty Version where
  pretty (Version ver c suf rev) =
    dispVer ver <> dispC c <> dispSuf suf <> dispRev rev
    where
      dispVer   = hcat . punctuate "." . map pretty
      dispC     = maybe emptyDoc pretty
      dispSuf   = hcat . map pretty
      dispRev 0 = emptyDoc
      dispRev n = "-r" <> pretty n

-- | 'Version' parser using 'Parsec'.
instance Parsec Version where
  parsec = do
    ver <- P.sepByNonEmpty digits (P.char '.')
    c   <- P.optional P.lower
    suf <- P.many parsec
    rev <- P.option 0 $ P.string "-r" *> digits
    return $ Version (NE.toList ver) c suf rev

-- | Check if the ebuild is a live ebuild, i.e. if its 'Version' is @9999@.
--
-- foo-9999* is treated as live ebuild
-- Cabal-1.17.9999* as well
--
-- >>> let (c,s,r) = (Nothing,[],0)
-- >>> is_live (Version [1,0,0] c s r)
-- False
-- >>> is_live (Version [999] c s r)
-- False
-- >>> is_live (Version [1,0,0,9999] c s r)
-- True
-- >>> is_live (Version [9999] c s r)
-- True
--
-- $
-- prop> \verNum char rev -> is_live (Version verNum char [] rev) == if length verNum >= 1 && last verNum >= 9999 then True else False
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

-- | Various allowed suffixes in Portage versions.
data Suffix = Alpha Int | Beta Int | Pre Int | RC Int | P Int
  deriving (Eq, Ord, Show, Read)

instance NFData Suffix where
  rnf (Alpha n) = rnf n
  rnf (Beta n)  = rnf n
  rnf (Pre n)   = rnf n
  rnf (RC n)    = rnf n
  rnf (P n)     = rnf n

instance Pretty Suffix where
  pretty suf = case suf of
    Alpha n -> "_alpha" <> dispPos n
    Beta n  -> "_beta"  <> dispPos n
    Pre n   -> "_pre"   <> dispPos n
    RC n    -> "_rc"    <> dispPos n
    P  n    -> "_p"     <> dispPos n

    where
      dispPos :: Int -> Doc ann
      dispPos 0 = emptyDoc
      dispPos n = pretty n

instance Parsec Suffix where
  parsec = P.char '_'
       *> P.choice
    [ P.string "alpha"       >> fmap Alpha maybeDigits
    , P.string "beta"        >> fmap Beta  maybeDigits
    , P.try (P.string "pre") >> fmap Pre   maybeDigits
    , P.string "rc"          >> fmap RC    maybeDigits
    , P.string "p"           >> fmap P     maybeDigits
    ]
    where
      maybeDigits = P.option 0 digits

-- | Convert from a 'Cabal.Version' to a Portage 'Version'.
-- 
-- prop> \verNum -> fromCabalVersion (Cabal.mkVersion verNum) == Version verNum Nothing [] 0
fromCabalVersion :: Cabal.Version -> Version
fromCabalVersion cabal_version = Version (Cabal.versionNumbers cabal_version) Nothing [] 0

-- | Convert from a Portage 'Version' to a 'Cabal.Version'.
-- $
-- prop> \verNum char rev -> toCabalVersion (Version verNum char [] rev) == if char == Nothing then Just (Cabal.mkVersion verNum) else Nothing
toCabalVersion :: Version -> Maybe Cabal.Version
toCabalVersion (Version nums Nothing [] _) = Just (Cabal.mkVersion nums)
toCabalVersion _                           = Nothing

-- | Parser which munches digits.
digits :: P.CharParsing m => m Int
digits = read <$> P.some P.digit

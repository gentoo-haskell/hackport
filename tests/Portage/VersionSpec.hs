module Portage.VersionSpec ( spec
                           , AnySuffix(..)
                           , ComplexVersion(..)
                           ) where

import Test.Hspec
import Test.Hspec.QuickCheck
import Test.QuickCheck

import qualified Distribution.Version as Cabal

import Portage.Version

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
newtype AnySuffix = AnySuffix { getSuffix :: Suffix }
  deriving (Eq,Ord,Show)

-- TODO: arbitrarily generate suffix Ints.
instance Arbitrary AnySuffix where
  arbitrary = elements $ AnySuffix <$> [ Alpha 1
                                       , Beta  1
                                       , Pre   1
                                       , RC    1
                                       , P     1
                                       ]
    
newtype ComplexVersion = ComplexVersion { getVersion :: Version }
  deriving (Eq,Ord,Show)

instance Arbitrary ComplexVersion where
  arbitrary = do
    v <- listOf $ getNonNegative <$> (arbitrary :: Gen (NonNegative Int))
    c <- Just <$> choose ('a','z')
    s <- listOf $ getSuffix <$> (arbitrary :: Gen AnySuffix)
    (NonNegative r) <- arbitrary
    return $ ComplexVersion $ Version v c s r
--------------------------------------------------------------------------------

spec :: Spec
spec = do
  describe "is_live" $ do
    prop "determines if a Portage version is live" $ do
      \(ComplexVersion v) -> is_live v ==
        if length (versionNumber v)  >= 1 && last (versionNumber v) >= 9999
        then True else False
        
  describe "fromCabalVersion" $ do
    prop "converts from a Cabal version to a Portage version" $ do
      \verNum -> fromCabalVersion (Cabal.mkVersion verNum) == Version verNum Nothing [] 0
      
  describe "toCabalVersion" $ do
    prop "converts from a Portage version to a Cabal version" $ do
      \(ComplexVersion v) -> toCabalVersion v ==
        if versionChar v == Nothing
        then Just (Cabal.mkVersion (versionNumber v)) else Nothing

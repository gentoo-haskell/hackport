module QuickCheck.Instances where

import Test.QuickCheck

import Portage.Version (Suffix(..), Version(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
-- | Wrapper for 'Suffix', intended for use in an 'Arbitrary' instance
-- to return a single, valid 'Suffix'.
newtype AnySuffix = AnySuffix { getSuffix :: Suffix }
  deriving (Eq,Ord,Show)

-- | Wrapper For 'Version', intended for use in an 'Arbitrary' instance
-- where we want to generate the most complex 'Version's possible.
newtype ComplexVersion = ComplexVersion { getVersion :: Version }
  deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
-- | Return a single, valid 'AnySuffix'. Currently the version of the
-- 'AnySuffix' is hardcoded as @1@.
instance Arbitrary AnySuffix where
  arbitrary = elements $ AnySuffix <$> [ Alpha 1
                                       , Beta  1
                                       , Pre   1
                                       , RC    1
                                       , P     1
                                       ]

-- | Return a valid 'ComplexVersion' with a non-empty 'versionNumber',
-- an optional 'versionChar', a ['Suffix'] which may be empty, and
-- a 'NonNegative' 'versionRevision' which may be zero.
--
-- This is used to generate 'Version's of high complexity to
-- stress-test our parsers for a range of valid inputs.
instance Arbitrary ComplexVersion where
  arbitrary = do
    v <- listOf1 $ getNonNegative <$> arbitrary
    c <- Just <$> choose ('a','z')
    s <- listOf $ getSuffix <$> (arbitrary :: Gen AnySuffix)
    (NonNegative r) <- arbitrary
    return $ ComplexVersion $ Version v (if length v == 1 then Nothing else c) s r

module QuickCheck.Instances (ComplexVersion(..)) where

import Test.QuickCheck

import Portage.Version (Suffix(..), Version(..))

--------------------------------------------------------------------------------
-- Types
--------------------------------------------------------------------------------
-- | Wrapper for 'Suffix', intended for use in an 'Arbitrary' instance
-- to return a single, valid 'Suffix'.
newtype ValidSuffix = ValidSuffix { getSuffix :: Suffix }
  deriving (Eq,Ord,Show)

-- | Wrapper For 'Version', intended for use in an 'Arbitrary' instance
-- where we want to generate the most complex 'Version' possible.
newtype ComplexVersion = ComplexVersion { getVersion :: Version }
  deriving (Eq,Ord,Show)

--------------------------------------------------------------------------------
-- Instances
--------------------------------------------------------------------------------
-- | Return a single, valid 'ValidSuffix'.
instance Arbitrary ValidSuffix where
  arbitrary = oneof [ ValidSuffix . Alpha . getNonNegative <$> arbitrary
                    , ValidSuffix . Beta  . getNonNegative <$> arbitrary
                    , ValidSuffix . Pre   . getNonNegative <$> arbitrary
                    , ValidSuffix . RC    . getNonNegative <$> arbitrary
                    , ValidSuffix . P     . getNonNegative <$> arbitrary
                    ]
  
-- | Return a valid 'ComplexVersion' with a non-empty 'versionNumber',
-- an optional 'versionChar' if valid, a ['Suffix'] which may be empty,
-- and a 'NonNegative' 'versionRevision' which may be zero.
--
-- This is used to generate a 'Version' of high complexity to
-- stress-test our parsers for a range of valid inputs.
instance Arbitrary ComplexVersion where
  arbitrary = do
    v <- listOf1 $ getNonNegative <$> arbitrary
    c <- oneof $ [Just <$> choose ('a','z'), elements [Nothing]]
    s <- listOf $ getSuffix <$> arbitrary
    (NonNegative r) <- arbitrary
    return $ ComplexVersion $ Version v (if length v == 1 then Nothing else c) s r

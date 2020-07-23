module Merge.UtilsSpec (spec) where

import Test.Hspec
import Test.Hspec.QuickCheck

import qualified Data.Map.Strict as Map
import qualified Data.List as L
import Control.Applicative

import Error
import Merge.Utils
import Portage.PackageId

import qualified Distribution.Package            as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.Version            as Cabal
import           Distribution.Pretty             (prettyShow)

spec :: Spec
spec = do
  describe "readPackageString" $ do
    context "when the package string is valid" $ do
      prop "returns a Right tuple containing the parsed information" $ do
        \ver -> readPackageString ["dev-haskell/packagename1" ++
                                   if ver == []
                                   then ""
                                   -- abs prevents negative version numbers in this test
                                   else "-" ++ prettyShow (Cabal.mkVersion (abs <$> ver))]
                == Right ( Just (Category "dev-haskell")
                         , Cabal.mkPackageName "packagename1"
                         , if ver == []
                           then Nothing
                           else Just (Version (abs <$> ver) Nothing [] 0)
                         )
    context "when the package string is empty" $ do
      it "returns a Left HackPortError" $ do
        readPackageString []
          `shouldBe`
          Left (ArgumentError "Need an argument, [category/]package[-version]")
    context "when the package string contains too many arguments" $ do
      it "returns a Left HackPortError" $ do
        let args = ["dev-haskell/packagename1-1.0.0", "dev-haskell/packagename2-1.0.0"]
        readPackageString args
          `shouldBe`
          Left (ArgumentError ("Too many arguments: " ++ unwords args))
          
  describe "getPreviousPackageId" $ do
    context "when there is a previous version available" $ do
      it "returns the PackageId of the previous version" $ do
        let ebuildDir = [ "foo-bar2-3.0.0b_rc2-r1.ebuild"
                        , "foo-bar2-3.0.0b_rc2-r2.ebuild"
                        , "foo-bar2-3.0.1.ebuild"
                        , "metadata.xml"
                        , "Manifest"
                        , "files"
                        ]
            newPkgId = PackageId (PackageName (Category "dev-haskell")
                                  (Cabal.mkPackageName "foo-bar2"))
                       (Version [3,0,2] Nothing [] 0 )
          in getPreviousPackageId ebuildDir newPkgId `shouldBe`
             Just (PackageId (PackageName (Category "dev-haskell")
                              (Cabal.mkPackageName "foo-bar2"))
                   (Version [3,0,1] Nothing [] 0))
    context "if there is no previous version available" $ do
      it "returns Nothing" $ do
        let ebuildDir = [ "foo-bar2-3.0.0b_rc2-r1.ebuild"
                        , "foo-bar2-3.0.0b_rc2-r2.ebuild"
                        , "foo-bar2-3.0.1.ebuild"
                        , "metadata.xml"
                        , "Manifest"
                        , "files"
                        ]
            newPkgId = PackageId (PackageName (Category "dev-haskell")
                                   (Cabal.mkPackageName "foo-bar2"))
                       (Version [3,0,0] (Just 'a') [] 0 )
          in getPreviousPackageId ebuildDir newPkgId `shouldBe` Nothing

  describe "drop_prefix" $ do
    context "when an IUSE has a with/use prefix" $ do
      prop "drops the prefix" $ do
        let prefix   = ["with","use"]
            sep      = ["-","_"]
            prefixes = liftA2 (++) prefix sep
        \flag -> L.nub (drop_prefix <$> (liftA2 (++) prefixes [flag])) == [flag]
    context "when an IUSE has neither a with nor a use prefix" $ do
      prop "preserves the existing string" $ do
        \prefix flag -> L.nub (drop_prefix <$> (liftA2 (++) [prefix] ['-':flag])) ==
          if prefix == "with" || prefix == "use"
          then [flag]
          else [prefix ++ '-':flag]

  describe "mangle_iuse" $ do
    prop "converts underscores (_) into hyphens (-) and drops with/use prefixes" $ do
        \a -> mangle_iuse a == drop_prefix (map (\f -> if f == '_' then '-' else f) a)

  describe "to_unstable" $ do
    prop "creates an unstable keyword from a stable keyword, or preserves a mask" $ do
      \a -> to_unstable a ==
        case a of
          '~':_ -> a
          '-':_ -> a
          _ -> '~':a

  describe "metaFlags" $ do
    prop "converts a [Cabal.PackageFlag] into a Map.Map String String" $ do
      \name desc -> metaFlags [(Cabal.emptyFlag (Cabal.mkFlagName name))
                                { Cabal.flagDescription = desc }] ==
                    Map.fromList [(mangle_iuse name,desc)]

  

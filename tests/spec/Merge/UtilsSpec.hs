module Merge.UtilsSpec (spec) where

import           Test.Hspec
import           Test.Hspec.QuickCheck
import           Test.QuickCheck
import           QuickCheck.Instances

import           Control.Applicative (liftA2)
import qualified Data.Map.Strict as Map
import           Data.Maybe (catMaybes)
import qualified Data.List as L

import           Error
import           Merge.Utils
import           Portage.PackageId

import qualified Distribution.Package            as Cabal
import qualified Distribution.PackageDescription as Cabal
import           Distribution.Pretty (prettyShow)

spec :: Spec
spec = do
  describe "readPackageString" $ do
    prop "returns a Right tuple containing the parsed information or a Left ArgumentError" $ do
      let cat = Category "dev-haskell"
          name = Cabal.mkPackageName "package-name1"
        in \(ComplexVersion version) ->
             readPackageString [prettyShow cat ++ "/" ++ prettyShow name ++
                                 if (versionNumber version) == []
                                 then ""
                                 else "-" ++ prettyShow version]
             `shouldBe`
             if (versionChar     version) /= Nothing ||
                (versionSuffix   version) /= []      ||
                (versionRevision version) /= 0
             then Left (ArgumentError ("Could not parse [category/]package[-version]: "
                                        ++ prettyShow cat ++ "/" ++
                                        prettyShow name ++
                                        if (versionNumber version) == []
                                        then ""
                                        else "-" ++ prettyShow version))
             else Right ( Just cat
                        , name
                        , if (versionNumber version) == []
                          then Nothing
                          else Just version
                        )
    it "returns a Left HackPortError if the package string is empty" $ do
      readPackageString []
        `shouldBe`
        Left (ArgumentError "Need an argument, [category/]package[-version]")
    prop "returns a Left HackPortError if fed too many arguments" $ do
      \(NonEmpty args) ->
        if length args > 1
        then readPackageString args `shouldBe`
             Left (ArgumentError ("Too many arguments: " ++ unwords args))
        else readPackageString args `shouldNotBe`
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

  describe "squash_debug" $ do
    it "squashes debug-related flags under the debug global USE flag" $ do
      squash_debug "use-debug-foo" `shouldBe` "debug"
      squash_debug "debug-foo" `shouldBe` "debug"
      squash_debug "foo-debugger" `shouldBe` "debug"
    it "ignores debug-unrelated flags" $ do
      squash_debug "foo-bar" `shouldBe` "foo-bar"

  describe "convert_underscores" $ do
    it "converts underscores (_) into hyphens (-)" $ do
      convert_underscores "foo_bar" `shouldBe` "foo-bar"
    it "ignores mangling of separators other than underscores" $ do
      convert_underscores "foo-bar" `shouldBe` "foo-bar"
    it "ignores mangling of characters which are not separators" $ do
      convert_underscores "foobar" `shouldBe` "foobar"

  describe "mangle_iuse" $ do
    it "performs all IUSE mangling" $ do
      mangle_iuse "use_foo_bar" `shouldBe` "foo-bar"
      mangle_iuse "with-baz-quux" `shouldBe` "baz-quux"
      mangle_iuse "use_debugging-symbols" `shouldBe` "debug"
      mangle_iuse "foo-bar-baz_quux" `shouldBe` "foo-bar-baz-quux"

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

  describe "dropIfUseExpand" $ do
    it "drops a USE flag if it is a USE_EXPAND, otherwise preserves it" $ do
      let use_expands = ["cpu_flags_x86","cpu_flags_arm"]
          flags       = Cabal.emptyFlag . Cabal.mkFlagName <$>
                        [ "cpu_flags_x86_sse4_2"
                        , "foo"
                        , "bar"
                        , "baz"
                        , "cpu_flags_arm_v8"
                        ]
      Cabal.unFlagName . Cabal.flagName <$> catMaybes (dropIfUseExpand use_expands <$> flags)
        `shouldBe` ["foo","bar","baz"]

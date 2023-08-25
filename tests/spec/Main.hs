
module Main where

import qualified Test.Hspec as Hspec
import qualified Test.Tasty as Tasty
import Test.Tasty.Hspec (testSpec)

import qualified Merge.UtilsSpec
import qualified Portage.Dependency.PrintSpec
import qualified Portage.Metadata.RemoteIdSpec
import qualified Portage.CabalSpec
import qualified Portage.EBuildSpec
import qualified Portage.GHCCoreSpec
import qualified Portage.MetadataSpec
import qualified Portage.PackageIdSpec
import qualified Portage.VersionSpec

specs :: [(Tasty.TestName, Hspec.Spec)]
specs =
    [ ("merge", Merge.UtilsSpec.spec)
    , ("dependency printing", Portage.Dependency.PrintSpec.spec)
    , ("remote-id metadata", Portage.Metadata.RemoteIdSpec.spec)
    , ("cabal", Portage.CabalSpec.spec)
    , ("ebuild", Portage.EBuildSpec.spec)
    , ("ghc core", Portage.GHCCoreSpec.spec)
    , ("metadata", Portage.MetadataSpec.spec)
    , ("package-id", Portage.PackageIdSpec.spec)
    , ("version", Portage.VersionSpec.spec)
    ]

main :: IO ()
main = do
    hspecTrees <- traverse (uncurry testSpec) specs
    Tasty.defaultMain $ Tasty.testGroup "hackport"
        [ Tasty.testGroup "hspec" hspecTrees
        ]

import qualified Distribution.Package as Cabal

import Hackport.Env (runEnv)
import qualified Portage.Overlay as Portage
import qualified Portage.Resolve as Portage
import qualified Portage.PackageId as Portage
import qualified Portage.Host as Portage

import Test.HUnit

tests :: Test
tests = TestList [ TestLabel "resolve cabal" (test_resolveCategory "dev-haskell" "cabal")
                 , TestLabel "resolve ghc" (test_resolveCategory "dev-lang" "ghc")
                 , TestLabel "resolve Cabal" (test_resolveCategory "dev-haskell" "Cabal")
                 , TestLabel "resolve DaRcS" (test_resolveCategory "dev-vcs" "DaRcS")
                 ]

test_resolveCategory :: String -> String -> Test
test_resolveCategory cat pkg = TestCase $ do
  info <- runEnv Portage.getInfo () mempty
  portage <- Portage.loadLazy (Portage.portage_dir info)
  let cabal = Cabal.mkPackageName pkg
      hits = Portage.resolveFullPortageName portage cabal
      expected = Just (Portage.PackageName (Portage.Category cat) (Portage.normalizeCabalPackageName cabal))
  assertEqual ("expecting to find package " ++ pkg) expected hits

main :: IO ()
main = runTestTTAndExit tests

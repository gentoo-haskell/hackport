import Test.HUnit

import qualified Portage.Overlay as Portage
import qualified Portage.Resolve as Portage
import qualified Portage.PackageId as Portage

import qualified Distribution.Package as Cabal

tests = TestList [ TestLabel "resolve cabal" (test_resolveCategory "dev-haskell" "cabal")
                 , TestLabel "resolve ghc" (test_resolveCategory "dev-lang" "ghc")
                 ]

test_resolveCategory :: String -> String -> Test
test_resolveCategory cat pkg = TestCase $ do
  portage <- Portage.loadLazy "/usr/portage"
  let cabal = Cabal.PackageName pkg
      hits = Portage.resolveFullPortageName portage cabal
      expected = Just (Portage.PackageName (Portage.Category cat) cabal)
  assertEqual ("expecting to find package " ++ pkg) hits expected 

main = runTestTT tests

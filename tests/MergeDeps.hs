{-# LANGUAGE OverloadedStrings #-}
import Control.Monad (when)

import qualified Distribution.Package as Cabal

import qualified Portage.Dependency as Portage

import Test.HUnit

import qualified RunTests as RT

tests = TestList [ TestLabel "simple merge" test_merge1
                 ]

test_merge1 = map (\(a,b,c) -> assertEqual "excepting merge" c (a `Portage.mergeDeps` b)) cases
  where 
    cases = 
      [ (["foo/bar"],["foo/baz"],["foo/bar", "foo/baz"]) -- a /= b => a+b
      --, (["foo/bar","foo/baz"],["foo/baz"],["foo/bar","foo/baz"]) -- a == b => a
      --, (["foo/bar"],["use? (foo/baz)"],["foo/bar", "use? (foo/baz)"]) -- covered by 1st
      --, (["foo/bar"],["use? (foo/bar)"],["foo/bar"])  -- a <> (use? a) => a
      --, (["foo/bar"],["use? ( use2? (foo/bar))"],["foo/bar"])
      -- , ([!use? ("foo/bar")],["use? (foo/baz)"],["!use? (foo/bar)","use? (foo/baz)"]) -- extract dep
      ]

-- A <> B
-- rules: 
--   1). \forall b \in B: \exists a \in A a ~= b ->  {A <> B \ b}
--   2). \forall b \in B: \not \exists a \in A ~=b

main = RT.run_tests tests

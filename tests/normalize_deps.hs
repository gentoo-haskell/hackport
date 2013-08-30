import Control.Monad (forM_)
import Data.List

import Test.HUnit

import qualified Portage.Dependency as P
import qualified Portage.PackageId  as P
import qualified Portage.Use        as P
import qualified RunTests as RT

tests :: Test
tests = TestList [ TestLabel "normalize_in_use_and_top" test_normalize_in_use_and_top
                 ]

pnm :: P.PackageName
pnm = P.mkPackageName "dev-haskell" "mtl"
pnp :: P.PackageName
pnp = P.mkPackageName "dev-haskell" "parsec"
pnq :: P.PackageName
pnq = P.mkPackageName "dev-haskell" "quickcheck"

def_attr :: P.DAttr
def_attr = P.DAttr P.AnySlot []

p_v :: [Int] -> P.Version
p_v v = P.Version { P.versionNumber   = v
                  , P.versionChar     = Nothing
                  , P.versionSuffix   = []
                  , P.versionRevision = 0
                  }

d_all :: [P.Dependency] -> P.Dependency
d_all = P.DependAllOf
d_any :: [P.Dependency] -> P.Dependency
d_any = P.DependAnyOf

d_ge :: P.PackageName -> [Int] -> P.Dependency
d_ge pn v = P.Atom pn
                   (P.DRange (P.NonstrictLB $ p_v v) P.InfinityB)
                   def_attr

d_use :: P.Use -> P.Dependency -> P.Dependency
d_use u d = P.DependIfUse (P.Q $ P.mkUse u) d

test_normalize_in_use_and_top :: Test
test_normalize_in_use_and_top = TestCase $ do
    let deps  = [ ( d_all [ d_ge pnm [1,0]
                          , d_use "foo" (d_all [ d_ge pnm [1,0] -- duplicate
                                               , d_ge pnp [2,1]
                                               ])
                          ]
                  , [ ">=dev-haskell/mtl-1.0"
                    , "foo? ( >=dev-haskell/parsec-2.1 )"
                    ]
                  )
                , ( d_all [ d_ge pnm [1,0]
                          , d_use "foo" (d_any [ d_ge pnm [1,0] -- already satisfied
                                               , d_ge pnp [2,1]
                                               ])
                          ]
                  , [ ">=dev-haskell/mtl-1.0" ]
                  )
                , ( d_any [ d_all [ d_ge pnm [1,0] -- common subdep
                                  , d_ge pnq [1,2]
                                  ]
                          , d_all [ d_ge pnm [1,0]
                                  , d_ge pnp [3,1]
                                  ]
                          ]
                  , [ ">=dev-haskell/mtl-1.0"
                    , "|| ( >=dev-haskell/quickcheck-1.2"
                    , "     >=dev-haskell/parsec-3.1 )"
                    ]
                  )
                ]
    forM_ deps $ \(d, expected) ->
        let actual = P.dep2str 0 d
        in assertEqual ("expecting empty result for " ++ show d)
                       (intercalate "\n" expected)
                       actual

main :: IO ()
main = RT.run_tests tests

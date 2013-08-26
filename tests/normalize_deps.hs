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

test_normalize_in_use_and_top :: Test
test_normalize_in_use_and_top = TestCase $ do
    let pnm = P.mkPackageName "dev-haskell" "mtl"
        pnp = P.mkPackageName "dev-haskell" "parsec"
        def_attr = P.DAttr P.AnySlot []
        p_v v = P.Version { P.versionNumber   = v
                          , P.versionChar     = Nothing
                          , P.versionSuffix   = []
                          , P.versionRevision = 0
                          }
        d_all = P.DependAllOf
        d_any = P.DependAnyOf
        d_ge pn v = P.Atom pn
                           (P.DRange (P.NonstrictLB $ p_v v) P.InfinityB)
                           def_attr
        d_use u d = P.DependIfUse (P.mkUse u) d
        deps  = [ ( d_all [ d_ge pnm [1,0]
                          , d_use "foo" (d_all [ d_ge pnm [1,0] -- duplicate
                                               , d_ge pnp [2,1]
                                               ])
                          ]
                  , [ ">=dev-haskell/mtl-1.0"
                    , "foo ( >=dev-haskell/parsec-2.1 )"
                    ]
                  )
                , ( d_all [ d_ge pnm [1,0]
                          , d_use "foo" (d_any [ d_ge pnm [1,0] -- already satisfied
                                               , d_ge pnp [2,1]
                                               ])
                          ]
                  , [ ">=dev-haskell/mtl-1.0" ]
                  )
                ]
    forM_ deps $ \(d, expected) ->
        let actual = P.dep2str 0 d
        in assertEqual ("expecting empty result for " ++ show d)
                       (intercalate "\n" expected)
                       actual

main :: IO ()
main = RT.run_tests tests

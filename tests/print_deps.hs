import Control.Monad (forM_)
import Data.List

import Test.HUnit

import qualified Portage.Dependency as P
import qualified Portage.PackageId  as P
import qualified Portage.Use        as P
import qualified Portage.Version    as P
import qualified RunTests as RT

tests = TestList [ TestLabel "print_empty" test_print_empty
                 , TestLabel "print_mixed" test_print_mixed
                 ]

test_print_empty :: Test
test_print_empty = TestCase $ do
    let expect_empty = ""
        d_all = P.DependAllOf
        d_any = P.DependAnyOf
        d_use u dep = P.DependIfUse (P.mkUse u) dep
        deps     = [ d_all []
                   , d_any []
                   , d_use "f" (d_all [])
                   , P.DependAllOf [d_any []]
                   , P.DependAnyOf [d_all []]
                   -- Deep Useless Use Tree :]
                   , d_use "f" $
                       d_use "g" $
                           d_all [d_any []
                                 , d_use "h" $
                                     d_all [ d_all []
                                           , d_any []
                                           ]
                                 , d_use "i" $
                                     d_all [ d_all []
                                           , d_any []
                                           ]
                                 ]
                   ]
    forM_ deps $ \d ->
        let actual_result = P.dep2str 0 d
        in assertEqual ("expecting empty result for " ++ show d)
                       expect_empty
                       actual_result

test_print_mixed :: Test
test_print_mixed = TestCase $ do
    let pn = P.mkPackageName "dev-haskell" "mtl"
        def_attr = P.DAttr P.AnySlot []
        p_v v = P.Version { P.versionNumber   = v
                          , P.versionChar     = Nothing
                          , P.versionSuffix   = []
                          , P.versionRevision = 0
                          }
        d_all = P.DependAllOf
        d_any = P.DependAnyOf
        d_ge v = P.Atom pn
                        (P.DRange (P.NonstrictLB $ p_v v) P.InfinityB)
                        def_attr
        d_lt v = P.Atom pn
                        (P.DRange P.ZeroB (P.StrictUB $ p_v v))
                        def_attr
        deps  = [ -- from agda: "mtl ==2.0.* || >=2.1.1 && <2.2"
                  ( d_all [ d_any [ d_all [ d_ge [2, 0]
                                          , d_lt [2, 1]
                                          ]
                                  , d_all [ d_ge [2, 1, 1]
                                          , d_lt [2, 2]
                                          ]
                                  ]
                          ]
                  , [ "|| ( ( >=dev-haskell/mtl-2.0 <dev-haskell/mtl-2.1 )"
                    , "     ( >=dev-haskell/mtl-2.1.1 <dev-haskell/mtl-2.2 ) )"
                    ]
                  )
                ]
    forM_ deps $ \(d, expected) ->
        let actual = P.dep2str 0 d
        in assertEqual ("expecting empty result for " ++ show d)
                       actual
                       (intercalate "\n" expected)


main = RT.run_tests tests
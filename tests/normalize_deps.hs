import Control.Monad (forM_)
import Data.List

import Test.HUnit

import qualified Portage.Dependency as P
import qualified Portage.Dependency.Normalize as PN
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

d_p :: String -> P.Dependency
d_p pn = P.Atom (P.mkPackageName "c" pn)
                (P.DRange P.ZeroB P.InfinityB)
                def_attr

d_use :: String -> P.Dependency -> P.Dependency
d_use u d = P.mkUseDependency (True, P.Use u) d

d_nuse :: String -> P.Dependency -> P.Dependency
d_nuse u d = P.mkUseDependency (False, P.Use u) d

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
                    , "|| ( >=dev-haskell/parsec-3.1"
                    , "     >=dev-haskell/quickcheck-1.2 )"
                    ]
                  )
                , ( d_all [ d_use "foo" $ d_use "bar" $ d_ge pnm [1,0]
                          , d_use "bar" $ d_use "foo" $ d_ge pnm [1,0]
                          ]
                  , [ "bar? ( foo? ( >=dev-haskell/mtl-1.0 ) )" ]
                  )
                , ( d_all [ d_use "foo" $ d_ge pnm [1,0]
                          , d_use "foo" $ d_ge pnp [3,1]
                          ]
                  , [ "foo? ( >=dev-haskell/mtl-1.0"
                    , "       >=dev-haskell/parsec-3.1 )"
                    ]
                  )
                , ( d_all [ d_use  "a" $ d_use "b" $ d_ge pnm [1,0]
                          , d_nuse "a" $ d_use "b" $ d_ge pnm [1,0]
                          ]
                  , [ "b? ( >=dev-haskell/mtl-1.0 )"
                    ]
                  )
                -- 'test?' is special
                , ( d_use "a" $ d_use "test" $ d_ge pnm [1,0]
                  , [ "test? ( a? ( >=dev-haskell/mtl-1.0 ) )"
                    ]
                  )
                , -- pop context for complementary depends, like
                  --   a? ( x y a ) !a? ( x y na )
                  -- leads to
                  --   x y a? ( a ) !a? ( na )
                  ( d_all [ d_use  "a" $ d_all $ map d_p [ "x", "y", "a" ]
                          , d_nuse "a" $ d_all $ map d_p [ "x", "y", "na" ]
                          ]
                  , [ "c/x"
                    , "c/y"
                    , "a? ( c/a )"
                    , "!a? ( c/na )"
                    ]
                  )
                , -- push stricter context into less strict
                  ( d_all [ d_ge pnm [2,0]
                          , d_use "a" $ d_ge pnm [1,0]
                          ]
                  ,
                    [ ">=dev-haskell/mtl-2.0" ]
                  )
                , -- propagate use guarded depend into deeply nested one
                  ( d_all [ d_use  "a"    $             d_all $ map d_p [ "x" ]
                          , d_use  "test" $ d_use "a" $ d_all $ map d_p [ "x", "t" ]
                          ]
                  , [ "test? ( a? ( c/t ) )"
                    , "a? ( c/x )"
                    ]
                  )
                , -- lift nested use context for complementary depends
                  --   a? ( b? ( y ) ) b? ( x )
                  ( d_all [ d_use  "a" $ d_use "b" $ d_all $ map d_p [ "x", "y" ]
                          , d_nuse "a" $ d_use "b" $ d_p "x"
                          ]
                  , [ "a? ( b? ( c/y ) )"
                    , "b? ( c/x )"
                    ]
                  )
                , -- more advanced lift of complementary deps
                  -- a? ( b? ( x y ) )
                  -- !a? ( b? ( y z ) )
                  ( d_all [ d_use   "a" $ d_use "b" $ d_all $ map d_p [ "x", "y" ]
                          , d_nuse  "a" $ d_use "b" $ d_all $ map d_p [ "y", "z" ]
                          ]
                  , [ "a? ( b? ( c/x ) )"
                    , "!a? ( b? ( c/z ) )"
                    , "b? ( c/y )"
                    ]
                  )
                , -- completely expanded set of USEs
                  -- a? ( b? ( c? ( x y z ) )
                  -- a? ( b? ( !c? ( x y ) )
                  -- a? ( !b? ( c? ( x z ) )
                  -- a? ( !b? ( !c? ( x ) )
                  --
                  -- !a? ( b? ( c? ( y z ) )
                  -- !a? ( b? ( !c? ( y ) )
                  -- !a? ( !b? ( c? ( z ) )
                  -- !a? ( !b? ( !c? ( ) )
                  ( d_all [ d_use   "a" $ d_use  "b" $ d_use  "c" $ d_all $ map d_p [ "x", "y", "z" ]
                          , d_use   "a" $ d_use  "b" $ d_nuse "c" $ d_all $ map d_p [ "x", "y" ]
                          , d_use   "a" $ d_nuse "b" $ d_use  "c" $ d_all $ map d_p [ "x", "z" ]
                          , d_use   "a" $ d_nuse "b" $ d_nuse "c" $ d_all $ map d_p [ "x" ]
                          , d_nuse  "a" $ d_use  "b" $ d_use  "c" $ d_all $ map d_p [ "y", "z" ]
                          , d_nuse  "a" $ d_use  "b" $ d_nuse "c" $ d_all $ map d_p [ "y" ]
                          , d_nuse  "a" $ d_nuse "b" $ d_use  "c" $ d_all $ map d_p [ "z" ]
                          , d_nuse  "a" $ d_nuse "b" $ d_nuse "c" $ d_all $ map d_p [ ]
                          ]
                  , [ "a? ( c/x )"
                    , "b? ( c/y )"
                    , "c? ( c/z )"
                    ]
                  )
                , -- pop simple common subdepend
                  -- a?  ( b? ( d ) )
                  -- !a? ( b? ( d ) )
                  ( d_all [ d_use   "a" $ d_use  "b" $ d_p "d"
                          , d_nuse  "a" $ d_use  "b" $ d_p "d"
                          ]
                  , [ "b? ( c/d )"
                    ]
                  )
                ]
    forM_ deps $ \(d, expected) ->
        let actual = P.dep2str 0 $ PN.normalize_depend d
        in assertEqual ("expecting matching result for " ++ show d)
                       (intercalate "\n" expected)
                       actual

main :: IO ()
main = RT.run_tests tests

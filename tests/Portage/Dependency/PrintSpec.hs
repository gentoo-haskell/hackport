module Portage.Dependency.PrintSpec (spec) where

import Test.Hspec

import Control.Monad (forM_)
import Data.List (intercalate)

import qualified Portage.PackageId            as P

import qualified Portage.Dependency.Builder   as P
import qualified Portage.Dependency.Print     as P
import qualified Portage.Dependency.Types     as P
import qualified Portage.Dependency.Normalize as P

import qualified Portage.Use                  as P

spec :: Spec
spec = do
  describe "dep2str" $ do
    it "converts an empty Dependency into an empty string" $ do
      let d_all = P.DependAllOf
          d_any = P.DependAnyOf
          d_use u dep = P.mkUseDependency (True, P.Use u) dep
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
        P.dep2str 0 (P.normalize_depend d) `shouldBe` ""

    it "converts a Dependency into a normalized string" $ do
      let def_attr = P.DAttr P.AnySlot []
          p_v v = P.Version { P.versionNumber   = v
                            , P.versionChar     = Nothing
                            , P.versionSuffix   = []
                            , P.versionRevision = 0
                            }
          d_all = P.DependAllOf
          d_any = P.DependAnyOf
          d_ge pn v = P.DependAtom $ P.Atom pn
                   (P.DRange (P.NonstrictLB $ p_v v) P.InfinityB)
                   def_attr
          d_lt pn v = P.DependAtom $ P.Atom pn
                   (P.DRange P.ZeroB (P.StrictUB $ p_v v))
                   def_attr
          -- for the deps_norm test, formerly normalize_deps
          pnm = P.mkPackageName "dev-haskell" "mtl"
          pnp = P.mkPackageName "dev-haskell" "parsec"
          pnq = P.mkPackageName "dev-haskell" "quickcheck"
          d_p pn = P.DependAtom $ P.Atom (P.mkPackageName "c" pn)
                   (P.DRange P.ZeroB P.InfinityB)
                   def_attr
          d_use u d = P.mkUseDependency (True, P.Use u) d
          d_nuse u d = P.mkUseDependency (False, P.Use u) d

          deps  = [ -- from agda: "mtl ==2.0.* || >=2.1.1 && <2.2"
            ( d_all [ d_any [ d_all [ d_ge pnm [2, 0]
                                    , d_lt pnm [2, 1]
                                    ]
                            , d_all [ d_ge pnm [2, 1, 1]
                                    , d_lt pnm [2, 2]
                                    ]
                            ]
                    ]
            , [ "|| ( ( >=dev-haskell/mtl-2.0 <dev-haskell/mtl-2.1 )"
              , "     ( >=dev-haskell/mtl-2.1.1 <dev-haskell/mtl-2.2 ) )"
              ]
            )
              -- remove duplicate entries
            , ( let d = d_all [d_ge pnm [2, 0], d_lt pnm [2, 2]]
                in d_all [d, d]
              , [ ">=dev-haskell/mtl-2.0 <dev-haskell/mtl-2.2" ]
              )
            ]
      forM_ deps $ \(d,expected) ->
        P.dep2str 0 (P.normalize_depend d) `shouldBe` (intercalate "\n" expected)

      -- from normalize_deps.hs
      let deps_norm  = [ ( d_all [ d_ge pnm [1,0]
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
                       , -- pop '|| ( some thing )' depend
                         ( let any_part = d_any $ map d_p ["a", "b"] in
                             d_all [ d_use  "u" $ d_all [ any_part , d_p "z"]
                                   , d_nuse "u" $         any_part
                                   ]
                         , [ "|| ( c/a"
                           , "     c/b )"
                           , "u? ( c/z )"
                           ]
                         )
                       , -- simplify slightly more complex counterguard
                         --   v? ( c/d )
                         --   u? ( !v? ( c/d ) )
                         -- to
                         --   v? ( c/d )
                         --   u? ( c/d )
                         ( d_all [               d_use  "v" $ d_p "d"
                                 , d_use   "u" $ d_nuse "v" $ d_p "d"
                                 ]
                         , [ "u? ( c/d )"
                           , "v? ( c/d )"
                           ]
                         )
                       
                       , --   ffi? ( c/d c/e )
                         --   !ffi ( !gmp ( c/d c/e ) )
                         --   gmp? ( c/d c/e )
                         -- to
                         --   ( c/d c/e )
                         ( let de = d_all [ d_p "d" , d_p "e" ]
                           in d_all [ d_use "ffi" de
                                    , d_nuse "ffi" $ d_nuse "gmp" $ de
                                    , d_use  "gmp" $ de
                                    ]
                         , [ "c/d"
                           , "c/e"
                           ]
                         )
                       
                       {- TODO: another popular case
                       , -- simplify even more complex counterguard
                         --   u? ( c/d )
                         --   !u? ( v? ( c/d ) )
                         -- to
                         --   u? ( c/d )
                         --   v? ( c/d )
                       ( d_all [               d_use "u" $ d_p "d"
                               , d_nuse  "u" $ d_use "v" $ d_p "d"
                               ]
                       , [ "u? ( c/d )"
                       , "v? ( c/d )"
                       ]
                       )
                       -}
                       ]
      forM_ deps_norm $ \(d, expected) ->
        (P.dep2str 0 $ P.normalize_depend d) `shouldBe` (intercalate "\n" expected)

  describe "dep2str_noindent" $ do
    it "converts a Dependency into a string" $ do
      let pn = P.mkPackageName "dev-haskell" "mtl"
          def_attr = P.DAttr P.AnySlot []
          p_v v = P.Version { P.versionNumber   = v
                            , P.versionChar     = Nothing
                            , P.versionSuffix   = []
                            , P.versionRevision = 0
                            }
          d_all = P.DependAllOf
          d_any = P.DependAnyOf
          d_ge v = P.DependAtom $ P.Atom pn
                   (P.DRange (P.NonstrictLB $ p_v v) P.InfinityB)
                   def_attr
          d_lt v = P.DependAtom $ P.Atom pn
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
            , [ "|| ( ( >=dev-haskell/mtl-2.0"
              , "       <dev-haskell/mtl-2.1 )"
              , "     ( >=dev-haskell/mtl-2.1.1"
              , "       <dev-haskell/mtl-2.2 ) )"
              ]
            )
              -- remove duplicate entries
            , ( let d = d_all [d_ge [2, 0], d_lt [2, 2]]
                in d_all [d, d]
              , [ ">=dev-haskell/mtl-2.0"
                , "<dev-haskell/mtl-2.2"
                , ">=dev-haskell/mtl-2.0"
                , "<dev-haskell/mtl-2.2"
                ]
              )
            ]
      forM_ deps $ \(d, expected) ->
        P.dep2str_noindent d `shouldBe` intercalate "\n" expected

import Control.Monad (forM_)

import Test.HUnit

import qualified Portage.Dependency as Portage
import qualified Portage.Use        as Portage
import qualified RunTests as RT

tests = TestList [ TestLabel "print_empty" test_print_empty
                 ]

test_print_empty :: Test
test_print_empty = TestCase $ do
    let expect_empty = ""
        d_all = Portage.DependAllOf
        d_any = Portage.DependAnyOf
        d_use u dep = Portage.DependIfUse (Portage.mkUse u) dep
        deps     = [ d_all []
                   , d_any []
                   , d_use "f" (d_all [])
                   , Portage.DependAllOf [d_any []]
                   , Portage.DependAnyOf [d_all []]
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
        let actual_result = Portage.dep2str 0 d
        in assertEqual ("expecting empty result for " ++ show d)
                       expect_empty
                       actual_result

main = RT.run_tests tests

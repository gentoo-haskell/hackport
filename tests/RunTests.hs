module RunTests
    (run_tests) where

import Control.Monad (when)

import System.Exit (exitFailure)
import Test.HUnit

something_broke :: Counts -> Bool
something_broke stats = errors stats + failures stats > 0

run_tests :: Test -> IO ()
run_tests tests =
    do stats <- runTestTT tests
       when (something_broke stats) exitFailure

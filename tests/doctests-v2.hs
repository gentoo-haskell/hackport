{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Monad.Fail (fail)
import Data.Foldable (for_)
import System.Exit (ExitCode (..), exitWith)
import System.Process (readProcess, createProcess, proc, waitForProcess, getProcessExitCode)

#if !MIN_VERSION_base(4,13,0)
import Prelude hiding (fail)
#endif


main :: IO ()
main = do
    doctestPath <- head . lines <$> readProcess "cabal" ["list-bin", "doctest"] []

    let components =
            [ "hackport:lib:hackport-internal"
            , "hackport:exe:hackport"
            ]

    for_ components $ \component -> do
        let cabalArgs =
                [ "repl"
                , "--with-compiler=" ++ doctestPath
                , component
                ]
        putStrLn $ "Running: cabal " ++ unwords cabalArgs
        (_, _, _, processHandle) <- createProcess (proc "cabal" cabalArgs)
        waitForProcess processHandle
        exitCode <- getProcessExitCode processHandle
        case exitCode of
            Nothing -> fail "No exit code from cabal process..."
            Just ExitSuccess -> pure ()
            Just c@(ExitFailure i) -> do
                putStrLn $ "\nFailure: cabal process returned exit code " ++ show i
                exitWith c

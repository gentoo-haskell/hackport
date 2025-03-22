{-# LANGUAGE CPP #-}

module Main (main) where

import Data.Foldable (for_)
import System.Exit (ExitCode (..), exitWith)
import System.Process (createProcess, proc, waitForProcess, getProcessExitCode)

#ifdef EXTERNAL_DOCTEST
import System.Directory (findExecutable)
#else
import System.Process (readProcess)
#endif


main :: IO ()
main = do

#ifdef EXTERNAL_DOCTEST
    doctestPath <- findExecutable "doctest"
        >>= maybe (error "Cannot find doctest exe") pure
#else
    doctestPath:_ <- lines <$> readProcess "cabal" ["list-bin", "doctest"] []
#endif

    let components =
            [ "hackport:lib:hackport-internal"
            , "hackport:exe:hackport"
            ]

    for_ components $ \component -> do
        let cabalArgs =
                [ "repl"
                  -- -Wtype-defaults screws up some doctests when run with -Werror
                , "--ghc-option=-Wno-type-defaults"
                , "--with-compiler=" ++ doctestPath
                , component
                ]
        putStrLn $ "Running: cabal " ++ unwords cabalArgs
        (_, _, _, processHandle) <- createProcess (proc "cabal" cabalArgs)
        _ <- waitForProcess processHandle
        exitCode <- getProcessExitCode processHandle
        case exitCode of
            Nothing -> fail "No exit code from cabal process..."
            Just ExitSuccess -> pure ()
            Just c@(ExitFailure i) -> do
                putStrLn $ "\nFailure: cabal process returned exit code " ++ show i
                exitWith c

module Main where

import System.Environment
import qualified Distribution.PackageDescription as Cabal
import Distribution.Verbosity (normal)
import Cabal2Ebuild

main :: IO ()
main = do
  args <- getArgs
  case args of
    [cabalFileName] -> do
      pkg <- Cabal.readPackageDescription normal cabalFileName
      let ebuild = cabal2ebuild (Cabal.flattenPackageDescription pkg)
      let ebuildFileName = name ebuild ++ "-" ++ version ebuild ++ ".ebuild"
      writeFile ebuildFileName (showEBuild ebuild)
    _ -> putStrLn "usage: cabal2ebuild package.cabal"

module Main where

import System.Environment

import Distribution.PackageDescription
import Distribution.PackageDescription.Parse

import Distribution.Text
import Distribution.Verbosity

import Portage.GHCCore

main :: IO ()
main = do
  args <- getArgs
  gpds <- mapM (readPackageDescription silent) args
  mapM_ guess gpds

guess :: GenericPackageDescription -> IO ()
guess gpd = do
  --gpd <- readPackageDescription verbose fp
  let pkg = package . packageDescription $ gpd
  let mghc = minimumGHCVersionToBuildPackage gpd
  putStr (display pkg)
  putStr "\t\t"
  putStrLn $ case mghc of
              Nothing -> "Unknown"
              Just v -> display v

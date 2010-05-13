{-# OPTIONS -XPatternGuards #-}

module DistroMap
  ( distroMap ) where

import Distribution.Verbosity
import Distribution.Client.Types
import Network.URI

distroMap :: Verbosity -> Repo -> FilePath -> FilePath -> [String] -> IO ()
distroMap verbosity repo portagePath overlayPath args = do
  putStrLn "distro map called"
  putStrLn ("verbosity: " ++ show verbosity)
  putStrLn ("portage: " ++ portagePath)
  putStrLn ("overlay: " ++ overlayPath)
  putStrLn ("args: " ++ show args)

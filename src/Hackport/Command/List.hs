module Hackport.Command.List
  ( listAction
  ) where

import qualified Distribution.Package as Cabal
import Distribution.Pretty (prettyShow)

import qualified Distribution.Client.Types as CabalInstall
import qualified Distribution.Client.IndexUtils as CabalInstall
import qualified Distribution.Solver.Types.SourcePackage as CabalInstall
import qualified Distribution.Solver.Types.PackageIndex as CabalInstall

import Portage.Overlay as Overlay (loadLazy, inOverlay)
import Portage.PackageId (normalizeCabalPackageId)
import Overlays

import Hackport.Util (withHackportContext)
import Hackport.Env

listAction :: Env ListEnv ()
listAction = do
  (GlobalEnv verbosity _ _, ListEnv pkgList) <- ask
  withHackportContext $ \_ repoContext -> do
    overlayPath <- getOverlayPath
    index <- fmap CabalInstall.packageIndex
      $ liftIO $ CabalInstall.getSourcePackages verbosity repoContext
    overlay <- Overlay.loadLazy overlayPath
    let pkgs | null pkgList = CabalInstall.allPackages index
             | otherwise = concatMap (concatMap snd . CabalInstall.searchByNameSubstring index) pkgList
        normalized = map (normalizeCabalPackageId . CabalInstall.srcpkgPackageId) pkgs
    let decorated = map (\p -> (Overlay.inOverlay overlay p, p)) normalized
    mapM_ (liftIO . putStrLn . pretty) decorated
  where
    pretty :: (Bool, Cabal.PackageIdentifier) -> String
    pretty (isInOverlay, pkgId) =
        let dec | isInOverlay = " * "
                | otherwise   = "   "
          in dec ++ prettyShow pkgId

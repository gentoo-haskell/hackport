{-# LANGUAGE ScopedTypeVariables #-}

module Overlays
    ( getOverlayPath
    ) where

import Control.Monad
import Control.Monad.State.Strict (MonadState)
import Data.List (nub, inits)
import Data.Maybe (maybeToList, listToMaybe, isJust, fromJust)
import qualified System.Directory as SD
import System.FilePath ((</>), splitPath, joinPath)

import Error
import Portage.Host
import Util (info)
import Hackport.Env hiding (local)

-- getOverlayPath :: forall m. (HasGlobalEnv m, MonadIO m) => m String
getOverlayPath :: Env env String
getOverlayPath = askGlobalEnv >>= \(GlobalEnv _ override_overlay _) -> do
  overlays <- if isJust override_overlay
                  then do info $ "Forced " ++ fromJust override_overlay
                          return [fromJust override_overlay]
                  else getOverlays
  case overlays of
    [] -> throw NoOverlay
    [x] -> return x
    mul -> search mul
  where
  search :: [String] -> Env env String
  search mul = do
    let loop [] = throw (MultipleOverlays mul)
        loop (x:xs) = do
          info $ "Checking '" ++ x ++ "'..."
          found <- liftIO $ SD.doesDirectoryExist (x </> ".hackport")
          if found
            then do
              info "OK!"
              return x
            else do
              info "Not ok."
              loop xs
    info "There are several overlays in your configuration."
    mapM_ (info . (" * " ++)) mul
    info "Looking for one with a HackPort cache..."
    overlay <- loop mul
    info $ "I choose " ++ overlay
    info "Override my decision with hackport --overlay /my/overlay"
    return overlay

getOverlays :: (HasGlobalEnv m, MonadIO m, MonadState WarningBuffer m)
    => m [String]
getOverlays = do
  local    <- getLocalOverlay
  overlays <- overlay_list <$> getInfo
  return $ nub $ map clean $
                 maybeToList local
              ++ overlays
  where
  clean path = case reverse path of
                '/':p -> reverse p
                _ -> path

getLocalOverlay :: MonadIO m => m (Maybe FilePath)
getLocalOverlay = do
  curDir <- liftIO $ SD.getCurrentDirectory
  let lookIn = map joinPath . reverse . inits . splitPath $ curDir
  fmap listToMaybe (filterM probe lookIn)

  where
    probe dir = liftIO $ SD.doesDirectoryExist (dir </> "dev-haskell")

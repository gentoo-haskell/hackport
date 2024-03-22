module Hackport.Dirs
  ( hackportDir
  ) where

import Control.Monad.IO.Class
import System.Directory
    ( getHomeDirectory
    , createDirectoryIfMissing
    )
import System.FilePath ( (</>) )

-- | Return the path to @~/.hackport/@ and create it if it doesn't exist.
hackportDir :: MonadIO m => m FilePath
hackportDir = do
    h <- liftIO $ getHomeDirectory
    let d = h </> ".hackport"
    liftIO $ createDirectoryIfMissing True d
    pure d

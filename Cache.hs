module Cache where

import CacheFile
import Error
import Index
import P2
import Version
import Overlays

import Distribution.Text ( simpleParse )

import Control.Arrow
import Data.Char
import Data.List
import Network.URI (URI, uriPath)
import Network.HTTP (Request(..), RequestMethod(GET), simpleHTTP, rspBody)
import qualified Data.ByteString.Lazy as L
import System.Time
import System.FilePath
import Control.Monad.Writer
import System.Directory (createDirectoryIfMissing)

import qualified Data.Map as Map

-- cabal
import Distribution.Verbosity

-- | A long time
alarmingLongTime :: TimeDiff
alarmingLongTime = TimeDiff
	{ tdYear = 0
	, tdMonth = 0
	, tdDay = 7
	, tdHour = 0
	, tdMin = 0
	, tdSec = 0
	, tdPicosec = 0
	}

updateCache :: Verbosity -> URI -> IO ()
updateCache verbose uri = do
  path <- getOverlayPath verbose
  let cache = cacheURI uri
  res <- simpleHTTP (Request cache GET [] "") -- `sayNormal` ("Fetching cache from "++show cache++"...",const "done.")
  case res of
    Left err -> throwEx (ConnectionFailed (show cache) (show err))
    Right resp -> do
      createDirectoryIfMissing False (path </> hackportDir)
      Prelude.writeFile (cacheFile path) (rspBody resp)
  where
  cacheURI :: URI -> URI
  cacheURI uri = uri {uriPath = uriPath uri </> indexFile}


readCache :: FilePath -> IO Index
readCache portdir = do
  let cachePath = cacheFile portdir
  -- TODO: re-implement
  -- exists <- doesFileExist cachePath
  -- unless exists $ do
  --   info "No cache file present, attempting to update..."
  --   updateCache
  str <- L.readFile cachePath
  return (readIndex str)

readDefaultCache :: Verbosity -> IO Index
readDefaultCache verbose = getOverlayPath verbose >>= readCache

indexToPortage :: Index -> Portage -> (Portage, [String])
indexToPortage index port = second nub . runWriter $ do
    pkgs <- forM index $ \(pkg_h_name, pkg_h_ver, pkg_desc) -> do
        let pkg_name = map toLower pkg_h_name
        pkg_cat <- lookupCat pkg_name
        Just ver <- return . simpleParse $ pkg_h_ver
        return $ Ebuild (P pkg_cat pkg_name)
                        (fromCabalVersion ver)
                        "<hackage>"
                        (Just pkg_desc)
    return $ Map.map sort $ Map.fromListWith (++) [ (ePackage e, [e]) | e <- pkgs ]
    where
    catMap = Map.fromListWith (++) [ (p, [c]) | P c p <- Map.keys port ]
    lookupCat :: String -> Writer [String] String
    lookupCat p = do
        case Map.lookup p catMap of
            Nothing -> return "hackage"
            Just [x] -> return x
            Just xs -> do
                let c | elem "dev-haskell" xs = "dev-haskell"
                      | otherwise = head xs
                tell ["WARNING: Category clash for package " ++ p ++ ", defaulting to " ++ c ++ ". Other categories: " ++ unwords (delete c xs)]
                return c

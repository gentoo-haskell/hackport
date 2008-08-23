module Cache where

import Action
import CacheFile
import Config
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
import Control.Monad.Error(throwError)
import Control.Monad.Writer
import Control.Monad (unless)
import System.Directory (doesFileExist,createDirectoryIfMissing)

import qualified Data.Map as Map

-- | A long time. Used in checkCacheDate
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

cacheURI :: URI -> URI
cacheURI uri = uri {uriPath = uriPath uri </> indexFile}

updateCache :: HPAction ()
updateCache = do
	path <- getOverlayPath
	cfg <- getCfg
	let cache = cacheURI $ server cfg
	res <- (liftIO $ simpleHTTP (Request cache GET [] "")) `sayNormal` ("Fetching cache from "++show cache++"...",const "done.")
	case res of
		Left err -> throwError (ConnectionFailed (show cache) (show err))
		Right resp -> liftIO $ do
                        createDirectoryIfMissing False (path </> hackportDir)
                        Prelude.writeFile (cacheFile path) (rspBody resp)

readCache :: FilePath -> HPAction Index
readCache portdir = do
	let cachePath = cacheFile portdir
	exists <- liftIO $ doesFileExist cachePath
	unless exists $ do
		info "No cache file present, attempting to update..."
		updateCache
	str <- liftIO $ L.readFile cachePath
	return $ readIndex str

readDefaultCache :: HPAction Index
readDefaultCache = do
    overlayPath <- getOverlayPath
    readCache overlayPath

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

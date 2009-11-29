module Main where

import Control.Monad
import Data.Char
import Data.Maybe
import Data.List
import Data.Monoid
        ( Monoid(..) )

-- cabal
import Distribution.Simple.Setup
        ( Flag(..), fromFlag
        , trueArg
        , flagToMaybe, flagToList
        , optionVerbosity
        )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.ReadE ( succeedReadE )
import Distribution.Simple.Command -- commandsRun
import Distribution.Simple.Utils ( die, cabalVersion )
-- import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Package as Cabal
import Distribution.Verbosity (Verbosity, normal)
import Distribution.Text (display)

import Distribution.Client.Types
import Distribution.Client.Update

import qualified Distribution.Client.PackageIndex as Index
import qualified Distribution.Client.IndexUtils as Index

import Portage.Overlay as Overlay ( loadLazy, inOverlay )
import Portage.Host as Host ( getInfo, portage_dir )
import Portage.PackageId ( normalizeCabalPackageId )

import Network.URI
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import System.FilePath ( (</>) )
import System.IO

import qualified Cabal2Ebuild as E
import Diff
import Error
import Status
import Overlays
import Merge

import Cabal2Ebuild

import qualified Paths_cabal_install 
import qualified Paths_hackport

-----------------------------------------------------------------------
-- List
-----------------------------------------------------------------------

data ListFlags = ListFlags {
    listVerbosity :: Flag Verbosity
    -- , listOverlayPath :: Flag FilePath
    -- , listServerURI :: Flag String
  }

instance Monoid ListFlags where
  mempty = ListFlags {
    listVerbosity = mempty
    -- , listOverlayPath = mempty
    -- , listServerURI = mempty
  }
  mappend a b = ListFlags {
    listVerbosity = combine listVerbosity
    -- , listOverlayPath = combine listOverlayPath
    -- , listServerURI = combine listServerURI
  }
    where combine field = field a `mappend` field b

defaultListFlags :: ListFlags
defaultListFlags = ListFlags {
    listVerbosity = Flag normal
    -- , listOverlayPath = NoFlag
    -- , listServerURI = Flag defaultHackageServerURI
  }

listCommand :: CommandUI ListFlags
listCommand = CommandUI {
    commandName = "list",
    commandSynopsis = "List packages",
    commandDescription = Just $ \_pname ->
        "TODO: this is the commandDescription for listCommand\n",
    commandUsage = usagePackages "list",
    commandDefaultFlags = defaultListFlags,
    commandOptions = \_showOrParseArgs ->
      [ optionVerbosity listVerbosity (\v flags -> flags { listVerbosity = v })
      {-
      , option [] ["overlay"]
         "Use cached packages list from specified overlay"
         listOverlayPath (\v flags -> flags { listOverlayPath = v })
         (reqArgFlag "PATH")
       -}
      ]
  }

listAction :: ListFlags -> [String] -> GlobalFlags -> IO ()
listAction flags extraArgs globalFlags = do
  let verbosity = fromFlag (listVerbosity flags)
  overlayPath <- getOverlayPath verbosity (fromFlag $ globalPathToOverlay globalFlags)
  let repo = defaultRepo overlayPath
  index <- fmap packageIndex (Index.getAvailablePackages verbosity [ repo ])
  overlay <- Overlay.loadLazy overlayPath
  let pkgs | null extraArgs = Index.allPackages index
           | otherwise = concatMap (Index.searchByNameSubstring index) extraArgs
      normalized = map (normalizeCabalPackageId . packageInfoId) pkgs
  let decorated = map (\p -> (Overlay.inOverlay overlay p, p)) normalized
  mapM_ (putStrLn . pretty) decorated
  where
  pretty :: (Bool, Cabal.PackageIdentifier) -> String
  pretty (isInOverlay, pkgId) =
      let dec | isInOverlay = " * "
              | otherwise   = "   "
      in dec ++ display pkgId


-----------------------------------------------------------------------
-- Make Ebuild
-----------------------------------------------------------------------

data MakeEbuildFlags = MakeEbuildFlags {
    makeEbuildVerbosity :: Flag Verbosity
  }

instance Monoid MakeEbuildFlags where
  mempty = MakeEbuildFlags {
    makeEbuildVerbosity = mempty
  }
  mappend a b = MakeEbuildFlags {
    makeEbuildVerbosity = combine makeEbuildVerbosity
  }
    where combine field = field a `mappend` field b

defaultMakeEbuildFlags :: MakeEbuildFlags
defaultMakeEbuildFlags = MakeEbuildFlags {
    makeEbuildVerbosity = Flag normal
  }

makeEbuildAction :: MakeEbuildFlags -> [String] -> GlobalFlags -> IO ()
makeEbuildAction flags args _globalFlags = do
  when (null args) $
    die "make-ebuild needs at least one argument"
  let _verbosity = fromFlag (makeEbuildVerbosity flags)
  forM_ args $ \cabalFileName -> do
    pkg <- Cabal.readPackageDescription normal cabalFileName
    let ebuild = cabal2ebuild (flattenPackageDescription pkg)
    let ebuildFileName = name ebuild ++ "-" ++ version ebuild ++ ".ebuild"
    writeFile ebuildFileName (showEBuild ebuild)

makeEbuildCommand :: CommandUI MakeEbuildFlags
makeEbuildCommand = CommandUI {
    commandName = "make-ebuild",
    commandSynopsis = "Make an ebuild from a .cabal file",
    commandDescription = Just $ \_pname ->
        "TODO: this is the commandDescription for makeEbuildCommand\n",
    commandUsage = \_ -> [],
    commandDefaultFlags = defaultMakeEbuildFlags,
    commandOptions = \_showOrParseArgs ->
      [ optionVerbosity makeEbuildVerbosity (\v flags -> flags { makeEbuildVerbosity = v })
      ]
  }

-----------------------------------------------------------------------
-- Diff
-----------------------------------------------------------------------

data DiffFlags = DiffFlags {
    -- diffMode :: Flag String, -- DiffMode,
    diffVerbosity :: Flag Verbosity
    -- , diffServerURI :: Flag String
  }

instance Monoid DiffFlags where
  mempty = DiffFlags {
    -- diffMode = mempty,
    diffVerbosity = mempty
    -- , diffServerURI = mempty
  }
  mappend a b = DiffFlags {
    -- diffMode = combine diffMode,
    diffVerbosity = combine diffVerbosity
    -- , diffServerURI = combine diffServerURI
  }
    where combine field = field a `mappend` field b

defaultDiffFlags :: DiffFlags
defaultDiffFlags = DiffFlags {
    -- diffMode = Flag "all",
    diffVerbosity = Flag normal
    -- , diffServerURI = Flag defaultHackageServerURI
  }

diffCommand :: CommandUI DiffFlags
diffCommand = CommandUI {
    commandName = "diff",
    commandSynopsis = "Run diff",
    commandDescription = Just $ \_pname ->
        "TODO: this is the commandDescription for diffCommand\n",
    commandUsage = usagePackages "diff",
    commandDefaultFlags = defaultDiffFlags,
    commandOptions = \_showOrParseArgs ->
      [ optionVerbosity diffVerbosity (\v flags -> flags { diffVerbosity = v })
      {-
      , option [] ["mode"]
         "Diff mode, one of: all, newer, missing, additions, common"
         diffMode (\v flags -> flags { diffMode = v })
         (reqArgFlag "MODE") -- I don't know how to map it strictly to DiffMode
      -}
      ]
  }

diffAction :: DiffFlags -> [String] -> GlobalFlags -> IO ()
diffAction flags args globalFlags = do
  let verbosity = fromFlag (diffVerbosity flags)
      -- dm0 = fromFlag (diffMode flags)
  dm <- case args of
          [] -> return ShowAll
          ["all"] -> return ShowAll
          ["missing"] -> return ShowMissing
          ["additions"] -> return ShowAdditions
          ["newer"] -> return ShowNewer
          ["common"] -> return ShowCommon
          ("package":  pkgs) -> return (ShowPackages pkgs)
          -- TODO: ["package",packagePattern] ->
          --          return ShowPackagePattern packagePattern
          _ -> die $ "Unknown mode: " ++ unwords args
  overlayPath <- getOverlayPath verbosity (fromFlag $ globalPathToOverlay globalFlags)
  let serverURI = defaultRepoURI overlayPath
      repo = defaultRepo overlayPath
  runDiff verbosity overlayPath dm repo

-----------------------------------------------------------------------
-- Update
-----------------------------------------------------------------------

data UpdateFlags = UpdateFlags {
    updateVerbosity :: Flag Verbosity
    -- , updateServerURI :: Flag String
  }

instance Monoid UpdateFlags where
  mempty = UpdateFlags {
    updateVerbosity = mempty
    -- , updateServerURI = mempty
  }
  mappend a b = UpdateFlags {
    updateVerbosity = combine updateVerbosity
    -- , updateServerURI = combine updateServerURI
  }
    where combine field = field a `mappend` field b

defaultUpdateFlags :: UpdateFlags
defaultUpdateFlags = UpdateFlags {
    updateVerbosity = Flag normal
    -- , updateServerURI = Flag defaultHackageServerURI
  }

updateCommand :: CommandUI UpdateFlags
updateCommand = CommandUI {
    commandName = "update",
    commandSynopsis = "Update the local cache",
    commandDescription = Just $ \_pname ->
        "TODO: this is the commandDescription for updateCommand\n",
    commandUsage = usageFlags "update",
    commandDefaultFlags = defaultUpdateFlags,
    commandOptions = \_ ->
      [ optionVerbosity updateVerbosity (\v flags -> flags { updateVerbosity = v })

      {-
      , option [] ["server"]
          "Set the server you'd like to update the cache from"
          updateServerURI (\v flags -> flags { updateServerURI = v} )
          (reqArgFlag "SERVER")
      -}
      ]
  }

updateAction :: UpdateFlags -> [String] -> GlobalFlags -> IO ()
updateAction flags extraArgs globalFlags = do
  unless (null extraArgs) $
    die $ "'update' doesn't take any extra arguments: " ++ unwords extraArgs
  let verbosity = fromFlag (updateVerbosity flags)
  overlayPath <- getOverlayPath verbosity (fromFlag $ globalPathToOverlay globalFlags)
  update verbosity [ defaultRepo overlayPath ]
  

-----------------------------------------------------------------------
-- Status
-----------------------------------------------------------------------

data StatusFlags = StatusFlags {
    statusVerbosity :: Flag Verbosity,
    statusOverlayPath :: Flag FilePath,
    statusPortdirPath :: Flag FilePath,
    statusToPortage :: Flag Bool
  }

instance Monoid StatusFlags where
  mempty = StatusFlags {
    statusVerbosity = mempty,
    statusOverlayPath = mempty,
    statusPortdirPath = mempty,
    statusToPortage = mempty
  }
  mappend a b = StatusFlags {
    statusVerbosity = combine statusVerbosity,
    statusOverlayPath = combine statusOverlayPath,
    statusPortdirPath = combine statusPortdirPath,
    statusToPortage = combine statusToPortage
  }
    where combine field = field a `mappend` field b

defaultStatusFlags :: StatusFlags
defaultStatusFlags = StatusFlags {
    statusVerbosity = Flag normal,
    statusOverlayPath = NoFlag,
    statusPortdirPath = NoFlag,
    statusToPortage = Flag False
  }

statusCommand :: CommandUI StatusFlags
statusCommand = CommandUI {
    commandName = "status",
    commandSynopsis = "Show status(??)",
    commandDescription = Just $ \_pname ->
        "TODO: this is the commandDescription for statusCommand\n",
    commandUsage = usagePackages "status",
    commandDefaultFlags = defaultStatusFlags,
    commandOptions = \_ ->
      [ optionVerbosity statusVerbosity (\v flags -> flags { statusVerbosity = v })
      , option [] ["overlay"]
         "Compare using the specified overlay"
         statusOverlayPath (\v flags -> flags { statusOverlayPath = v })
         (reqArgFlag "PATH")

      , option [] ["portdir"]
         "Compare using the specified portdir"
         statusPortdirPath (\v flags -> flags { statusPortdirPath = v })
         (reqArgFlag "PATH")

      , option [] ["to-portage"]
          "Print only packages likely to be interesting to move to the portage tree."
          statusToPortage (\v flags -> flags { statusToPortage = v })
          trueArg
      ]
  }

statusAction :: StatusFlags -> [String] -> GlobalFlags -> IO ()
statusAction flags args globalFlags = do
  let verbosity = fromFlag (statusVerbosity flags)
      overlayPathM = flagToMaybe (statusOverlayPath flags)
      portdirM = flagToMaybe (statusPortdirPath flags)
      toPortdir = fromFlag (statusToPortage flags)
  portdir <- maybe (Host.portage_dir `fmap` Host.getInfo) return portdirM
  overlayPath <- maybe (getOverlayPath verbosity (fromFlag $ globalPathToOverlay globalFlags)) return overlayPathM
  runStatus verbosity portdir overlayPath toPortdir args

-----------------------------------------------------------------------
-- Merge
-----------------------------------------------------------------------

data MergeFlags = MergeFlags {
    mergeVerbosity :: Flag Verbosity
    -- , mergeServerURI :: Flag String
  }

instance Monoid MergeFlags where
  mempty = MergeFlags {
    mergeVerbosity = mempty
    -- , mergeServerURI = mempty
  }
  mappend a b = MergeFlags {
    mergeVerbosity = combine mergeVerbosity
    -- , mergeServerURI = combine mergeServerURI
  }
    where combine field = field a `mappend` field b

defaultMergeFlags :: MergeFlags
defaultMergeFlags = MergeFlags {
    mergeVerbosity = Flag normal
    -- , mergeServerURI = Flag defaultHackageServerURI
  }

mergeCommand :: CommandUI MergeFlags
mergeCommand = CommandUI {
    commandName = "merge",
    commandSynopsis = "Make an ebuild out of hackage package",
    commandDescription = Just $ \_pname ->
      "TODO: this is the commandDescription for mergeCommand\n",
    commandUsage = usagePackages "merge",
    commandDefaultFlags = defaultMergeFlags,
    commandOptions = \_showOrParseArgs ->
      [ optionVerbosity mergeVerbosity (\v flags -> flags { mergeVerbosity = v })

      {-
      , option [] ["server"]
          "Set the server you'd like to update the cache from"
          mergeServerURI (\v flags -> flags { mergeServerURI = v} )
          (reqArgFlag "SERVER")
      -}
      ]
  }

mergeAction :: MergeFlags -> [String] -> GlobalFlags -> IO ()
mergeAction flags extraArgs globalFlags = do
  let verbosity = fromFlag (mergeVerbosity flags)
  overlayPath <- getOverlayPath verbosity (fromFlag $ globalPathToOverlay globalFlags)
  let repo = defaultRepo overlayPath
  merge verbosity repo (defaultRepoURI overlayPath) extraArgs overlayPath

-----------------------------------------------------------------------
-- Utils
-----------------------------------------------------------------------

defaultRepo :: FilePath -> Repo
defaultRepo overlayPath =
  Repo {
      repoKind = Left hackage,
      repoLocalDir = overlayPath </> ".hackport"
    }
  where
    hackage = RemoteRepo server_name uri
    server_name = "hackage.haskell.org"
    uri  = URI "http:" (Just (URIAuth "" server_name "")) "/packages/archive" "" ""

defaultRepoURI :: FilePath -> URI
defaultRepoURI overlayPath =
  case repoKind (defaultRepo overlayPath) of
    Left (RemoteRepo { remoteRepoURI = uri }) -> uri

getServerURI :: String -> IO URI
getServerURI str =
  case parseURI str of
    Just uri -> return uri
    Nothing -> throwEx (InvalidServer str)

reqArgFlag :: ArgPlaceHolder -> SFlags -> LFlags -> Description ->
              (b -> Flag String) -> (Flag String -> b -> b) -> OptDescr b
reqArgFlag ad = reqArg ad (succeedReadE Flag) flagToList

usagePackages :: String -> String -> String
usagePackages op_name pname =
  "Usage: " ++ pname ++ " " ++ op_name ++ " [FLAGS] [PACKAGE]\n\n"
  ++ "Flags for " ++ op_name ++ ":"

usageFlags :: String -> String -> String
usageFlags flag_name pname =
      "Usage: " ++ pname ++ " " ++ flag_name ++ " [FLAGS]\n\n"
      ++ "Flags for " ++ flag_name ++ ":"

-----------------------------------------------------------------------
-- Main
-----------------------------------------------------------------------

data GlobalFlags =
    GlobalFlags { globalVersion :: Flag Bool
                , globalNumericVersion :: Flag Bool
                , globalPathToOverlay :: Flag (Maybe FilePath)
                }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags =
    GlobalFlags { globalVersion = Flag False
                , globalNumericVersion = Flag False
                , globalPathToOverlay = Flag Nothing
                }

globalCommand :: CommandUI GlobalFlags
globalCommand = CommandUI {
    commandName = "",
    commandSynopsis = "",
    commandDescription = Just $ \_pname ->
        "TODO: this is the commandDescription for globalCommand\n",
    commandUsage = \_ -> [],
    commandDefaultFlags = defaultGlobalFlags,
    commandOptions = \_showOrParseArgs ->
        [ option ['V'] ["version"]
            "Print version information"
            globalVersion (\v flags -> flags { globalVersion = v })
            trueArg
        , option [] ["numeric-version"]
            "Print just the version number"
            globalNumericVersion (\v flags -> flags { globalNumericVersion = v })
            trueArg
        , option ['p'] ["overlay-path"]
            "Override search path list where .hackport/ lives (default list: ['.', paludis-ovls or emerge-ovls])"
            globalPathToOverlay (\path flags -> flags { globalPathToOverlay = path })
            (reqArg' "PATH" (Flag . Just) (\(Flag ms) -> catMaybes [ms])) -- FIXME: i should be optional
        ]
    }

mainWorker :: [String] -> IO ()
mainWorker args =
  case commandsRun globalCommand commands args of
    CommandHelp help -> printHelp help
    CommandList opts -> printOptionsList opts
    CommandErrors errs -> printErrors errs
    CommandReadyToGo (globalflags, commandParse) -> do
      case commandParse of
        _ | fromFlag (globalVersion globalflags)        -> printVersion
          | fromFlag (globalNumericVersion globalflags) -> printNumericVersion
        CommandHelp help        -> printHelp help
        CommandList opts        -> printOptionsList opts
        CommandErrors errs      -> printErrors errs
        CommandReadyToGo action -> catchEx (action globalflags) errorHandler
    where
    printHelp help = getProgName >>= putStr . help
    printOptionsList = putStr . unlines
    printErrors errs = do
      putStr (concat (intersperse "\n" errs))
      exitFailure
    printNumericVersion = putStrLn $ display Paths_hackport.version
    printVersion        = putStrLn $ "hackport version "
                                  ++ display Paths_hackport.version
                                  ++ "\nusing cabal-install "
                                  ++ display Paths_cabal_install.version
                                  ++ " and the Cabal library version "
                                  ++ display cabalVersion
    errorHandler :: HackPortError -> IO ()
    errorHandler e = do
      putStrLn (hackPortShowError e)
    commands =
      [ listCommand `commandAddAction` listAction
      , makeEbuildCommand `commandAddAction` makeEbuildAction
      , statusCommand `commandAddAction` statusAction
      , diffCommand `commandAddAction` diffAction
      , updateCommand `commandAddAction` updateAction
      , mergeCommand `commandAddAction` mergeAction
      ]

main :: IO ()
main = getArgs >>= mainWorker

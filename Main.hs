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
        , falseArg 
        , flagToMaybe, flagToList
        , optionVerbosity
        )
import Distribution.PackageDescription.Configuration
         ( flattenPackageDescription )
import Distribution.ReadE ( succeedReadE )
import Distribution.Simple.Command -- commandsRun
import Distribution.Simple.Utils ( die )
import qualified Distribution.PackageDescription as Cabal
import Distribution.Verbosity (Verbosity, normal)

import Network.URI
import System.Environment ( getArgs, getProgName )
import System.Exit ( exitFailure )
import System.IO

import Bash
import qualified Cabal2Ebuild as E
import Cache
import Diff
import Error
import Index
import Status
import Overlays
import Merge

import Cabal2Ebuild

-----------------------------------------------------------------------
-- List
-----------------------------------------------------------------------

data ListFlags = ListFlags {
    listVerbosity :: Flag Verbosity,
    listOverlayPath :: Flag FilePath
  }

instance Monoid ListFlags where
  mempty = ListFlags {
    listVerbosity = mempty,
    listOverlayPath = mempty
  }
  mappend a b = ListFlags {
    listVerbosity = combine listVerbosity,
    listOverlayPath = combine listOverlayPath
  }
    where combine field = field a `mappend` field b

defaultListFlags :: ListFlags
defaultListFlags = ListFlags {
    listVerbosity = Flag normal,
    listOverlayPath = NoFlag
  }

listCommand :: CommandUI ListFlags
listCommand = CommandUI {
    commandName = "list",
    commandSynopsis = "List packages",
    commandDescription = Just $ \pname ->
        "TODO: this is the commandDescription for listCommand\n",
    commandUsage = usagePackages "list",
    commandDefaultFlags = defaultListFlags,
    commandOptions = \showOrParseArgs ->
      [ optionVerbosity listVerbosity (\v flags -> flags { listVerbosity = v })
      , option [] ["overlay"]
         "Use cached packages list from specified overlay"
         listOverlayPath (\v flags -> flags { listOverlayPath = v })
         (reqArgFlag "PATH")
      ]
  }

listAction :: ListFlags -> [String] -> GlobalFlags -> IO ()
listAction flags args globalFlags = do
  let verbose = fromFlag (listVerbosity flags)
      portdirM = flagToMaybe (listOverlayPath flags)
  overlay <- maybe (getOverlayPath verbose) return portdirM
  index <- readCache overlay
  let index' | null name = index
             | otherwise = filterIndexByPV matchSubstringCaseInsensitive index
      pkgs = [ pkg ++ "-" ++ ver | (pkg,ver,_) <- index']
  if null pkgs
    then throwEx (PackageNotFound name)
    else putStr . unlines . sort $ pkgs
      where
      name | null args = []
           | otherwise = head args
      matchSubstringCaseInsensitive otherName _pVver =
           map toLower name `isInfixOf` map toLower otherName

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
makeEbuildAction flags args globalFlags = do
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
    commandDescription = Just $ \pname ->
        "TODO: this is the commandDescription for makeEbuildCommand\n",
    commandUsage = \_ -> [],
    commandDefaultFlags = defaultMakeEbuildFlags,
    commandOptions = \showOrParseArgs ->
      [ optionVerbosity makeEbuildVerbosity (\v flags -> flags { makeEbuildVerbosity = v })
      ]
  }

-----------------------------------------------------------------------
-- Diff
-----------------------------------------------------------------------

data DiffFlags = DiffFlags {
    diffMode :: Flag DiffMode,
    diffVerbosity :: Flag Verbosity
  }

instance Monoid DiffFlags where
  mempty = DiffFlags {
    diffMode = mempty,
    diffVerbosity = mempty
  }
  mappend a b = DiffFlags {
    diffMode = combine diffMode,
    diffVerbosity = combine diffVerbosity
  }
    where combine field = field a `mappend` field b

defaultDiffFlags :: DiffFlags
defaultDiffFlags = DiffFlags {
    diffMode = Flag ShowAll,
    diffVerbosity = Flag normal
  }

diffCommand :: CommandUI DiffFlags
diffCommand = CommandUI {
    commandName = "diff",
    commandSynopsis = "Run diff",
    commandDescription = Just $ \pname ->
        "TODO: this is the commandDescription for diffCommand\n",
    commandUsage = usagePackages "diff",
    commandDefaultFlags = defaultDiffFlags,
    commandOptions = \showOrParseArgs ->
      [ optionVerbosity diffVerbosity (\v flags -> flags { diffVerbosity = v })
      ]
  }

diffAction :: DiffFlags -> [String] -> GlobalFlags -> IO ()
diffAction flags args globalFlags = do
  let verbose = fromFlag (diffVerbosity flags)
      dm = fromFlag (diffMode flags)
  overlayPath <- getOverlayPath verbose
  runDiff verbose overlayPath dm

-----------------------------------------------------------------------
-- Update
-----------------------------------------------------------------------

data UpdateFlags = UpdateFlags {
    updateVerbosity :: Flag Verbosity,
    updateServerURI :: Flag String
  }

instance Monoid UpdateFlags where
  mempty = UpdateFlags {
    updateVerbosity = mempty,
    updateServerURI = mempty
  }
  mappend a b = UpdateFlags {
    updateVerbosity = combine updateVerbosity,
    updateServerURI = combine updateServerURI
  }
    where combine field = field a `mappend` field b

defaultUpdateFlags :: UpdateFlags
defaultUpdateFlags = UpdateFlags {
    updateVerbosity = Flag normal,
    updateServerURI = Flag defaultHackageServerURI
  }

updateCommand :: CommandUI UpdateFlags
updateCommand = CommandUI {
    commandName = "update",
    commandSynopsis = "Update the local cache",
    commandDescription = Just $ \pname ->
        "TODO: this is the commandDescription for updateCommand\n",
    commandUsage = usageFlags "update",
    commandDefaultFlags = defaultUpdateFlags,
    commandOptions = \_ ->
      [ optionVerbosity updateVerbosity (\v flags -> flags { updateVerbosity = v })

      , option [] ["server"]
          "Set the server you'd like to update the cache from"
          updateServerURI (\v flags -> flags { updateServerURI = v} )
          (reqArgFlag "SERVER")
      ]
  }

updateAction :: UpdateFlags -> [String] -> GlobalFlags -> IO ()
updateAction flags args globalFlags = do
  let verbose = fromFlag (updateVerbosity flags)
      server  = fromFlag (updateServerURI flags)
  case parseURI server of
    Just uri -> updateCache verbose uri
    Nothing -> throwEx (InvalidServer server)

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
    commandDescription = Just $ \pname ->
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
          falseArg
      ]
  }

statusAction :: StatusFlags -> [String] -> GlobalFlags -> IO ()
statusAction flags args globalFlags = do
  let verbose = fromFlag (statusVerbosity flags)
      overlayPathM = flagToMaybe (statusOverlayPath flags)
      portdirM = flagToMaybe (statusPortdirPath flags)
      toPortdir = fromFlag (statusToPortage flags)
  portdir <- maybe getSystemPortdir return portdirM
  overlayPath <- maybe (getOverlayPath verbose) return overlayPathM
  runStatus verbose portdir overlayPath toPortdir

-----------------------------------------------------------------------
-- Merge
-----------------------------------------------------------------------

data MergeFlags = MergeFlags {
    mergeVerbosity :: Flag Verbosity,
    mergeServerURI :: Flag String
  }

instance Monoid MergeFlags where
  mempty = MergeFlags {
    mergeVerbosity = mempty,
    mergeServerURI = mempty
  }
  mappend a b = MergeFlags {
    mergeVerbosity = combine mergeVerbosity,
    mergeServerURI = combine mergeServerURI
  }
    where combine field = field a `mappend` field b

defaultMergeFlags :: MergeFlags
defaultMergeFlags = MergeFlags {
    mergeVerbosity = Flag normal,
    mergeServerURI = Flag defaultHackageServerURI
  }

mergeCommand :: CommandUI MergeFlags
mergeCommand = CommandUI {
    commandName = "merge",
    commandSynopsis = "Make an ebuild out of hackage package",
    commandDescription = Just $ \pname ->
      "TODO: this is the commandDescription for mergeCommand\n",
    commandUsage = usagePackages "merge",
    commandDefaultFlags = defaultMergeFlags,
    commandOptions = \showOrParseArgs ->
      [ optionVerbosity mergeVerbosity (\v flags -> flags { mergeVerbosity = v })

      , option [] ["server"]
          "Set the server you'd like to update the cache from"
          mergeServerURI (\v flags -> flags { mergeServerURI = v} )
          (reqArgFlag "SERVER")
      ]
  }

mergeAction :: MergeFlags -> [String] -> GlobalFlags -> IO ()
mergeAction flags [pkg] globalFlags = do
  let verbose = fromFlag (mergeVerbosity flags)
      server  = fromFlag (mergeServerURI flags)
  case parseURI server of
    Just uri -> merge verbose uri pkg
    Nothing -> throwEx (InvalidServer server)
  
mergeAction _ _ _ =
    throwEx (ArgumentError "'merge' needs exactly one parameter")

-----------------------------------------------------------------------
-- Utils
-----------------------------------------------------------------------

defaultHackageServerURI :: String
defaultHackageServerURI = "http://hackage.haskell.org/packages/archive/"

reqArgFlag :: ArgPlaceHolder -> SFlags -> LFlags -> Description ->
              (b -> Flag String) -> (Flag String -> b -> b) -> OptDescr b
reqArgFlag ad = reqArg ad (succeedReadE Flag) flagToList

usagePackages :: String -> String -> String
usagePackages name pname =
  "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS] [PACKAGE]\n\n"
  ++ "Flags for " ++ name ++ ":"

usageFlags :: String -> String -> String
usageFlags name pname =
      "Usage: " ++ pname ++ " " ++ name ++ " [FLAGS]\n\n"
      ++ "Flags for " ++ name ++ ":"

-----------------------------------------------------------------------
-- Main
-----------------------------------------------------------------------

data GlobalFlags = GlobalFlags {
    globalVersion :: Flag Bool
    }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags = GlobalFlags {
    globalVersion = Flag False
    }

globalCommand :: CommandUI GlobalFlags
globalCommand = CommandUI {
    commandName = "",
    commandSynopsis = "",
    commandDescription = Just $ \pname ->
        "TODO: this is the commandDescription for globalCommand\n",
    commandUsage = \_ -> [],
    commandDefaultFlags = defaultGlobalFlags,
    commandOptions = \showOrParseArgs ->
        [ ]
    }

mainWorker :: [String] -> IO ()
mainWorker args =
  case commandsRun globalCommand commands args of
    CommandHelp help -> printHelp help
    CommandList opts -> printOptionsList opts
    CommandErrors errs -> printErrors errs
    CommandReadyToGo (globalflags, commandParse) -> do
      case commandParse of 
        CommandHelp help -> printHelp help
        CommandList opts -> printOptionsList opts
        CommandErrors errs -> printErrors errs
        CommandReadyToGo action -> catchEx (action globalflags) errorHandler
    where
    printHelp help = getProgName >>= putStr . help
    printOptionsList = putStr . unlines
    printErrors errs = do
      putStr (concat (intersperse "\n" errs))
      exitFailure
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

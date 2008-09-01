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
    listVerbosity :: Flag Verbosity
  }

instance Monoid ListFlags where
  mempty = ListFlags {
    listVerbosity = mempty
  }
  mappend a b = ListFlags {
    listVerbosity = combine listVerbosity
  }
    where combine field = field a `mappend` field b

defaultListFlags :: ListFlags
defaultListFlags = ListFlags {
    listVerbosity = Flag normal
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
      ]
  }

listAction :: ListFlags -> [String] -> GlobalFlags -> IO ()
listAction flags args globalFlags = do
  let verbose = fromFlag (listVerbosity flags)
      portDirM = flagToMaybe (globalOverlayPath globalFlags)
  overlay <-
    case portDirM of
      Just dir -> return dir
      Nothing -> getOverlayPath verbose
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
      overlayPath = fromFlag (globalOverlayPath globalFlags)
      dm = fromFlag (diffMode flags)
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
    statusToPortage :: Flag Bool
  }

instance Monoid StatusFlags where
  mempty = StatusFlags {
    statusVerbosity = mempty,
    statusToPortage = mempty
  }
  mappend a b = StatusFlags {
    statusVerbosity = combine statusVerbosity,
    statusToPortage = combine statusToPortage
  }
    where combine field = field a `mappend` field b

defaultStatusFlags :: StatusFlags
defaultStatusFlags = StatusFlags {
    statusVerbosity = Flag normal,
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
    commandOptions = \showOrParseArgs ->
      [ optionVerbosity statusVerbosity (\v flags -> flags { statusVerbosity = v })

      , option [] ["to-portage"]
          "Print only packages likely to be interesting to move to the portage tree."
          statusToPortage (\v flags -> flags { statusToPortage = v })
          falseArg
        ]
  }

statusAction :: StatusFlags -> [String] -> GlobalFlags -> IO ()
statusAction flags args globalFlags = do
  let verbose = fromFlag (statusVerbosity flags)
      portDir = fromFlag (globalPortDir globalFlags)
      overlayPath = fromFlag (globalOverlayPath globalFlags)
      toPortDir = fromFlag (statusToPortage flags)
  runStatus verbose portDir overlayPath toPortDir

-----------------------------------------------------------------------
-- Merge
-----------------------------------------------------------------------

mergeCommand :: CommandUI (Flag String)
mergeCommand = CommandUI {
    commandName = "merge",
    commandSynopsis = "Make an ebuild out of hackage package",
    commandDescription = Just $ \pname ->
      "TODO: this is the commandDescription for mergeCommand\n",
    commandUsage = usagePackages "merge",
    commandDefaultFlags = Flag defaultHackageServerURI,
    commandOptions = \showOrParseArgs ->
      []
  }

mergeAction :: Flag String -> [String] -> GlobalFlags -> IO ()
mergeAction serverFlag [pkg] globalFlags = do
  let verbose = normal -- fromFlag (globalVerbosity globalFlags)
      server  = fromFlag serverFlag
  case parseURI server of
    Just uri -> merge verbose uri pkg
    Nothing -> throwEx (InvalidServer server)
  
mergeAction _ _ _ =
    throwEx (ArgumentError "'merge' needs exactly one parameter")

-----------------------------------------------------------------------
-- Main and utils
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

data GlobalFlags = GlobalFlags {
    globalVersion :: Flag Bool,
    globalOverlayPath :: Flag FilePath,
    globalPortDir :: Flag FilePath
    }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags = GlobalFlags {
    globalVersion = Flag False,
    globalOverlayPath = NoFlag,
    globalPortDir = NoFlag
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

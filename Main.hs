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

listCommand :: CommandUI ()
listCommand = CommandUI {
    commandName = "list",
    commandSynopsis = "List packages",
    commandDescription = Just $ \pname ->
        "TODO: this is the commandDescription for listCommand\n",
    commandUsage = usagePackages "list",
    commandDefaultFlags = (),
    commandOptions = \showOrParseArgs ->
      []
  }

listAction :: () -> [String] -> GlobalFlags -> IO ()
listAction _ args globalFlags = do
  let verbose = fromFlag (globalVerbosity globalFlags)
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

makeEbuildAction :: () -> [String] -> GlobalFlags -> IO ()
makeEbuildAction _ args globalFlags = do
  when (null args) $
    die "make-ebuild needs at least one argument"
  forM_ args $ \cabalFileName -> do
    pkg <- Cabal.readPackageDescription normal cabalFileName
    let ebuild = cabal2ebuild (flattenPackageDescription pkg)
    let ebuildFileName = name ebuild ++ "-" ++ version ebuild ++ ".ebuild"
    writeFile ebuildFileName (showEBuild ebuild)

makeEbuildCommand :: CommandUI ()
makeEbuildCommand = CommandUI {
    commandName = "make-ebuild",
    commandSynopsis = "Make an ebuild from a .cabal file",
    commandDescription = Just $ \pname ->
        "TODO: this is the commandDescription for makeEbuildCommand\n",
    commandUsage = \_ -> [],
    commandDefaultFlags = (),
    commandOptions = \showOrParseArgs ->
        []
  }

-----------------------------------------------------------------------
-- Diff
-----------------------------------------------------------------------

data DiffFlags = DiffFlags {
    diffMode :: Flag DiffMode
  }

instance Monoid DiffFlags where
  mempty = DiffFlags {
    diffMode = mempty
  }
  mappend a b = DiffFlags {
    diffMode = combine diffMode
  }
    where combine field = field a `mappend` field b

defaultDiffFlags :: DiffFlags
defaultDiffFlags = DiffFlags {
    diffMode = Flag ShowAll
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
       []
  }

diffAction :: DiffFlags -> [String] -> GlobalFlags -> IO ()
diffAction flags args globalFlags = do
  let verbose = fromFlag (globalVerbosity globalFlags)
      overlayPath = fromFlag (globalOverlayPath globalFlags)
      dm = fromFlag (diffMode flags)
  runDiff verbose overlayPath dm

-----------------------------------------------------------------------
-- Update
-----------------------------------------------------------------------

updateCommand :: CommandUI (Flag String)
updateCommand = CommandUI {
    commandName = "update",
    commandSynopsis = "Update the local cache",
    commandDescription = Just $ \pname ->
        "TODO: this is the commandDescription for updateCommand\n",
    commandUsage = usageFlags "update",
    commandDefaultFlags = Flag defaultHackageServerURI,
    commandOptions = \_ ->
      [ option [] ["server"]
          "Set the server you'd like to update the cache from"
          id (\v _ -> v)
          (reqArgFlag "SERVER")
      ]
  }

updateAction :: Flag String -> [String] -> GlobalFlags -> IO ()
updateAction serverFlag args globalFlags = do
  let verbose = fromFlag (globalVerbosity globalFlags)
      server  = fromFlag serverFlag
  case parseURI server of
    Just uri -> updateCache verbose uri
    Nothing -> throwEx (InvalidServer server)

-----------------------------------------------------------------------
-- Status
-----------------------------------------------------------------------

data StatusFlags = StatusFlags {
    statusToPortage :: Flag Bool
  }

instance Monoid StatusFlags where
  mempty = StatusFlags {
    statusToPortage = mempty
  }
  mappend a b = StatusFlags {
    statusToPortage = combine statusToPortage
  }
    where combine field = field a `mappend` field b

defaultStatusFlags :: StatusFlags
defaultStatusFlags = StatusFlags {
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
        [option [] ["to-portage"]
          "Print only packages likely to be interesting to move to the portage tree."
          statusToPortage (\v flags -> flags { statusToPortage = v })
          falseArg
        ]
  }

statusAction :: StatusFlags -> [String] -> GlobalFlags -> IO ()
statusAction flags args globalFlags = do
  let verbose = fromFlag (globalVerbosity globalFlags)
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
  let verbose = fromFlag (globalVerbosity globalFlags)
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
    globalPortDir :: Flag FilePath,
    globalVerbosity :: Flag Verbosity
    }

defaultGlobalFlags :: GlobalFlags
defaultGlobalFlags = GlobalFlags {
    globalVersion = Flag False,
    globalOverlayPath = NoFlag,
    globalPortDir = NoFlag,
    globalVerbosity = Flag normal
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
        [option "v" ["verbose"]
          globalVerbosity (\v flags -> flags { globalVerbosity = v } )
          falseArg
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

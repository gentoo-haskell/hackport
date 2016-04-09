module Main where

import Control.Applicative
import Control.Monad
import Data.Maybe
import Data.List
import Data.Monoid
        ( Monoid(..) )

-- cabal
import Distribution.Simple.Setup
        ( Flag(..), fromFlag
        , trueArg
        , flagToList
        , optionVerbosity
        )
import Distribution.ReadE ( succeedReadE )
import Distribution.Simple.Command -- commandsRun
import Distribution.Simple.Utils ( die, cabalVersion, warn )
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Package as Cabal
import Distribution.Verbosity (Verbosity, normal)
import Distribution.Text (display, simpleParse)

import Distribution.Client.Types
import Distribution.Client.Update
import qualified Distribution.Client.PackageIndex as Index
import qualified Distribution.Client.IndexUtils as Index

import Portage.Overlay as Overlay ( loadLazy, inOverlay )
import Portage.Host as Host ( getInfo, portage_dir )
import Portage.PackageId ( normalizeCabalPackageId )

import Network.URI ( URI(..), parseURI )
import System.Environment ( getArgs, getProgName )
import System.Directory ( doesDirectoryExist )
import System.Exit ( exitFailure )
import System.FilePath ( (</>) )

import qualified HackPort.GlobalFlags as H

import Diff
import Error
import Status
import Overlays
import Merge
import DistroMap ( distroMap )

import qualified Paths_cabal_install
import qualified Paths_hackport

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
    commandSynopsis = "List package versions matching pattern",
    commandUsage = usagePackages "list",
    commandDescription = Nothing,
    commandNotes = Nothing,

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

listAction :: ListFlags -> [String] -> H.GlobalFlags -> IO ()
listAction flags extraArgs globalFlags = do
 let verbosity = fromFlag (listVerbosity flags)
 H.withHackPortContext verbosity globalFlags $ \repoContext -> do
  overlayPath <- getOverlayPath verbosity (fromFlag $ H.globalPathToOverlay globalFlags)
  index <- fmap packageIndex (Index.getSourcePackages verbosity repoContext)
  overlay <- Overlay.loadLazy overlayPath
  let pkgs | null extraArgs = Index.allPackages index
           | otherwise = concatMap (concatMap snd . Index.searchByNameSubstring index) extraArgs
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
  , makeEbuildCabalFlags :: Flag (Maybe String)
  }

instance Monoid MakeEbuildFlags where
  mempty = MakeEbuildFlags {
    makeEbuildVerbosity = mempty
  , makeEbuildCabalFlags = mempty
  }
  mappend a b = MakeEbuildFlags {
    makeEbuildVerbosity = combine makeEbuildVerbosity
  , makeEbuildCabalFlags = makeEbuildCabalFlags b
  }
    where combine field = field a `mappend` field b

defaultMakeEbuildFlags :: MakeEbuildFlags
defaultMakeEbuildFlags = MakeEbuildFlags {
    makeEbuildVerbosity = Flag normal
  , makeEbuildCabalFlags = Flag Nothing
  }

makeEbuildAction :: MakeEbuildFlags -> [String] -> H.GlobalFlags -> IO ()
makeEbuildAction flags args globalFlags = do
  (catstr,cabals) <- case args of
                      (category:cabal1:cabaln) -> return (category, cabal1:cabaln)
                      _ -> throwEx (ArgumentError "make-ebuild needs at least two arguments. <category> <cabal-1> <cabal-n>")
  cat <- case simpleParse catstr of
            Just c -> return c
            Nothing -> throwEx (ArgumentError ("could not parse category: " ++ catstr))
  let verbosity = fromFlag (makeEbuildVerbosity flags)
  overlayPath <- getOverlayPath verbosity (fromFlag $ H.globalPathToOverlay globalFlags)
  forM_ cabals $ \cabalFileName -> do
    pkg <- Cabal.readPackageDescription normal cabalFileName
    mergeGenericPackageDescription verbosity overlayPath cat pkg False (fromFlag $ makeEbuildCabalFlags flags)

makeEbuildCommand :: CommandUI MakeEbuildFlags
makeEbuildCommand = CommandUI {
    commandName = "make-ebuild",
    commandSynopsis = "Make an ebuild from a .cabal file",
    commandUsage = \_ -> [],
    commandDescription = Nothing,
    commandNotes = Nothing,

    commandDefaultFlags = defaultMakeEbuildFlags,
    commandOptions = \_showOrParseArgs ->
      [ optionVerbosity makeEbuildVerbosity (\v flags -> flags { makeEbuildVerbosity = v })

      , option "f" ["flags"]
        (unlines [ "Set cabal flags to certain state."
                 , "Example: --flags=-all_extensions"
                 ])
        makeEbuildCabalFlags
        (\cabal_flags v -> v{ makeEbuildCabalFlags = cabal_flags })
        (reqArg' "cabal_flags" (Flag . Just) (\(Flag ms) -> catMaybes [ms]))
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
    commandUsage = usagePackages "diff",
    commandDescription = Nothing,
    commandNotes = Nothing,

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

diffAction :: DiffFlags -> [String] -> H.GlobalFlags -> IO ()
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
  overlayPath <- getOverlayPath verbosity (fromFlag $ H.globalPathToOverlay globalFlags)
  H.withHackPortContext verbosity globalFlags $ \repoContext ->
      runDiff verbosity overlayPath dm repoContext

-----------------------------------------------------------------------
-- Update
-----------------------------------------------------------------------

data UpdateFlags = UpdateFlags {
    updateVerbosity :: Flag Verbosity
  }

instance Monoid UpdateFlags where
  mempty = UpdateFlags {
    updateVerbosity = mempty
  }
  mappend a b = UpdateFlags {
    updateVerbosity = combine updateVerbosity
  }
    where combine field = field a `mappend` field b

defaultUpdateFlags :: UpdateFlags
defaultUpdateFlags = UpdateFlags {
    updateVerbosity = Flag normal
  }

updateCommand :: CommandUI UpdateFlags
updateCommand = CommandUI {
    commandName = "update",
    commandSynopsis = "Update the local package database",
    commandUsage = usageFlags "update",
    commandDescription = Nothing,
    commandNotes = Nothing,

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

updateAction :: UpdateFlags -> [String] -> H.GlobalFlags -> IO ()
updateAction flags extraArgs globalFlags = do
  unless (null extraArgs) $
    die $ "'update' doesn't take any extra arguments: " ++ unwords extraArgs
  let verbosity = fromFlag (updateVerbosity flags)

  H.withHackPortContext verbosity globalFlags $ \repoContext ->
    update verbosity repoContext

-----------------------------------------------------------------------
-- Status
-----------------------------------------------------------------------

data StatusFlags = StatusFlags {
    statusVerbosity :: Flag Verbosity,
    statusDirection :: Flag StatusDirection
  }

defaultStatusFlags :: StatusFlags
defaultStatusFlags = StatusFlags {
    statusVerbosity = Flag normal,
    statusDirection = Flag PortagePlusOverlay
  }

statusCommand :: CommandUI StatusFlags
statusCommand = CommandUI {
    commandName = "status",
    commandSynopsis = "Show up-to-date status against other repos (hackage, ::gentoo)",
    commandUsage = usagePackages "status",
    commandDescription = Nothing,
    commandNotes = Nothing,

    commandDefaultFlags = defaultStatusFlags,
    commandOptions = \_ ->
      [ optionVerbosity statusVerbosity (\v flags -> flags { statusVerbosity = v })
      , option [] ["to-portage"]
          "Print only packages likely to be interesting to move to the portage tree."
          statusDirection (\v flags -> flags { statusDirection = v })
          (noArg (Flag OverlayToPortage))
      , option [] ["from-hackage"]
          "Print only packages likely to be interesting to move from hackage tree."
          statusDirection (\v flags -> flags { statusDirection = v })
          (noArg (Flag HackageToOverlay))
      ]
  }

statusAction :: StatusFlags -> [String] -> H.GlobalFlags -> IO ()
statusAction flags args globalFlags = do
  let verbosity = fromFlag (statusVerbosity flags)
      direction = fromFlag (statusDirection flags)
  portagePath <- getPortageDir verbosity globalFlags
  overlayPath <- getOverlayPath verbosity (fromFlag $ H.globalPathToOverlay globalFlags)

  H.withHackPortContext verbosity globalFlags $ \repoContext ->
      runStatus verbosity portagePath overlayPath direction args repoContext

-----------------------------------------------------------------------
-- Merge
-----------------------------------------------------------------------

data MergeFlags = MergeFlags {
    mergeVerbosity :: Flag Verbosity
  , mergeCabalFlags :: Flag (Maybe String)
  }

instance Monoid MergeFlags where
  mempty = MergeFlags {
    mergeVerbosity = mempty
  , mergeCabalFlags = mempty
  }
  mappend a b = MergeFlags {
    mergeVerbosity = combine mergeVerbosity
  , mergeCabalFlags = mergeCabalFlags b
  }
    where combine field = field a `mappend` field b

defaultMergeFlags :: MergeFlags
defaultMergeFlags = MergeFlags {
    mergeVerbosity = Flag normal
  , mergeCabalFlags = Flag Nothing
  }

mergeCommand :: CommandUI MergeFlags
mergeCommand = CommandUI {
    commandName = "merge",
    commandSynopsis = "Make an ebuild out of hackage package",
    commandUsage = usagePackages "merge",
    commandDescription = Nothing,
    commandNotes = Nothing,

    commandDefaultFlags = defaultMergeFlags,
    commandOptions = \_showOrParseArgs ->
      [ optionVerbosity mergeVerbosity (\v flags -> flags { mergeVerbosity = v })

      , option "f" ["flags"]
        (unlines [ "Set cabal flags to certain state."
                 , "Example: --flags=-all_extensions"
                 ])
        mergeCabalFlags
        (\cabal_flags v -> v{ mergeCabalFlags = cabal_flags})
        (reqArg' "cabal_flags" (Flag . Just) (\(Flag ms) -> catMaybes [ms]))
      ]
  }

mergeAction :: MergeFlags -> [String] -> H.GlobalFlags -> IO ()
mergeAction flags extraArgs globalFlags = do
  let verbosity = fromFlag (mergeVerbosity flags)
  overlayPath <- getOverlayPath verbosity (fromFlag $ H.globalPathToOverlay globalFlags)

  H.withHackPortContext verbosity globalFlags $ \repoContext ->
    merge verbosity repoContext extraArgs overlayPath (fromFlag $ mergeCabalFlags flags)

-----------------------------------------------------------------------
-- DistroMap
-----------------------------------------------------------------------

data DistroMapFlags = DistroMapFlags {
    distroMapVerbosity :: Flag Verbosity
  }

instance Monoid DistroMapFlags where
  mempty = DistroMapFlags {
    distroMapVerbosity = mempty
    -- , mergeServerURI = mempty
  }
  mappend a b = DistroMapFlags {
    distroMapVerbosity = combine distroMapVerbosity
  }
    where combine field = field a `mappend` field b

defaultDistroMapFlags :: DistroMapFlags
defaultDistroMapFlags = DistroMapFlags {
    distroMapVerbosity = Flag normal
  }

distroMapCommand :: CommandUI DistroMapFlags
distroMapCommand = CommandUI {
    commandName = "distromap",
    commandSynopsis = "Build hackage a distromap file",
    commandUsage = usagePackages "distromap",
    commandDescription = Nothing,
    commandNotes = Nothing,

    commandDefaultFlags = defaultDistroMapFlags,
    commandOptions = \_showOrParseArgs ->
      [ optionVerbosity distroMapVerbosity (\v flags -> flags { distroMapVerbosity = v })
      ]
  }

distroMapAction :: DistroMapFlags-> [String] -> H.GlobalFlags -> IO ()
distroMapAction flags extraArgs globalFlags = do
  let verbosity = fromFlag (distroMapVerbosity flags)
  overlayPath <- getOverlayPath verbosity (fromFlag $ H.globalPathToOverlay globalFlags)
  portagePath <- getPortageDir verbosity globalFlags

  H.withHackPortContext verbosity globalFlags $ \repoContext ->
    distroMap verbosity repoContext portagePath overlayPath extraArgs

-----------------------------------------------------------------------
-- Utils
-----------------------------------------------------------------------

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

getPortageDir :: Verbosity -> H.GlobalFlags -> IO FilePath
getPortageDir verbosity globalFlags = do
  let portagePathM =  fromFlag (H.globalPathToPortage globalFlags)
  portagePath <- case portagePathM of
                   Nothing -> Host.portage_dir <$> Host.getInfo
                   Just path -> return path
  exists <- doesDirectoryExist $ portagePath </> "dev-haskell"
  when (not exists) $
    warn verbosity $ "Looks like an invalid portage directory: " ++ portagePath
  return portagePath

-----------------------------------------------------------------------
-- Main
-----------------------------------------------------------------------

globalCommand :: CommandUI H.GlobalFlags
globalCommand = CommandUI {
    commandName = "",
    commandSynopsis = "",
    commandDescription = Nothing,
    commandNotes = Nothing,
    commandUsage = \_ -> [],

    commandDefaultFlags = H.defaultGlobalFlags,
    commandOptions = \_showOrParseArgs ->
        [ option ['V'] ["version"]
            "Print version information"
            H.globalVersion (\v flags -> flags { H.globalVersion = v })
            trueArg
        , option [] ["numeric-version"]
            "Print just the version number"
            H.globalNumericVersion (\v flags -> flags { H.globalNumericVersion = v })
            trueArg
        , option ['p'] ["overlay-path"]
            "Override search path list where .hackport/ lives (default list: ['.', paludis-ovls or emerge-ovls])"
            H.globalPathToOverlay (\ovrl_path flags -> flags { H.globalPathToOverlay = ovrl_path })
            (reqArg' "PATH" (Flag . Just) (\(Flag ms) -> catMaybes [ms]))
        , option [] ["portage-path"]
            "Override path to your portage tree"
            H.globalPathToPortage (\port_path flags -> flags { H.globalPathToPortage = port_path })
            (reqArg' "PATH" (Flag . Just) (\(Flag ms) -> catMaybes [ms]))
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
        _ | fromFlag (H.globalVersion globalflags)        -> printVersion
          | fromFlag (H.globalNumericVersion globalflags) -> printNumericVersion
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
      , distroMapCommand `commandAddAction` distroMapAction
      ]

main :: IO ()
main = getArgs >>= mainWorker

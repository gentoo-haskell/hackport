{-# LANGUAGE CPP #-}

module Main (main) where

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
        , optionVerbosity
        )

import Distribution.Simple.Command -- commandsRun
import Distribution.Simple.Utils ( die, cabalVersion, warn )
import qualified Distribution.PackageDescription.Parse as Cabal
import qualified Distribution.Package as Cabal
import Distribution.Verbosity (Verbosity, normal)
import Distribution.Text (display, simpleParse)

import qualified Distribution.Client.Setup as CabalInstall
import qualified Distribution.Client.Types as CabalInstall
import qualified Distribution.Client.Update as CabalInstall

import qualified Distribution.Client.IndexUtils as CabalInstall
import qualified Distribution.Solver.Types.SourcePackage as CabalInstall
import qualified Distribution.Solver.Types.PackageIndex as CabalInstall

import Portage.Overlay as Overlay ( loadLazy, inOverlay )
import Portage.Host as Host ( getInfo, portage_dir )
import Portage.PackageId ( normalizeCabalPackageId )

import System.Environment ( getArgs, getProgName )
import System.Directory ( doesDirectoryExist )
import System.Exit ( exitFailure )
{-# LANGUAGE CPP #-}

import System.FilePath ( (</>) )

import qualified HackPort.GlobalFlags as H

import Error
import Status
import Overlays
import Merge

import qualified Paths_cabal_install
import qualified Paths_hackport

-----------------------------------------------------------------------
-- List
-----------------------------------------------------------------------

data ListFlags = ListFlags {
    listVerbosity :: Flag Verbosity
  }

#if MIN_VERSION_base(4,9,0)
instance Semigroup ListFlags where
  a <> b = ListFlags {
    listVerbosity = combine listVerbosity
  }
    where combine field = field a <> field b
#endif

instance Monoid ListFlags where
  mempty = ListFlags {
    listVerbosity = mempty
  }
#if !(MIN_VERSION_base(4,11,0))
  mappend a b = ListFlags {
    listVerbosity = combine listVerbosity
  }
    where combine field = field a `mappend` field b
#endif

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
  index <- fmap CabalInstall.packageIndex (CabalInstall.getSourcePackages verbosity repoContext)
  overlay <- Overlay.loadLazy overlayPath
  let pkgs | null extraArgs = CabalInstall.allPackages index
           | otherwise = concatMap (concatMap snd . CabalInstall.searchByNameSubstring index) extraArgs
      normalized = map (normalizeCabalPackageId . CabalInstall.packageInfoId) pkgs
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

#if MIN_VERSION_base(4,9,0)
instance Semigroup MakeEbuildFlags where
  a <> b = MakeEbuildFlags {
    makeEbuildVerbosity = combine makeEbuildVerbosity
  , makeEbuildCabalFlags = makeEbuildCabalFlags b
  }
    where combine field = field a <> field b
#endif

instance Monoid MakeEbuildFlags where
  mempty = MakeEbuildFlags {
    makeEbuildVerbosity = mempty
  , makeEbuildCabalFlags = mempty
  }
#if MIN_VERSION_base(4,9,0)
  mappend a b = MakeEbuildFlags {
    makeEbuildVerbosity = combine makeEbuildVerbosity
  , makeEbuildCabalFlags = makeEbuildCabalFlags b
  }
    where combine field = field a `mappend` field b
#endif

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
-- Update
-----------------------------------------------------------------------

data UpdateFlags = UpdateFlags {
    updateVerbosity :: Flag Verbosity
  }

#if MIN_VERSION_base(4,9,0)
instance Semigroup UpdateFlags where
  a <> b = UpdateFlags {
    updateVerbosity = combine updateVerbosity
  }
    where combine field = field a <> field b
#endif

instance Monoid UpdateFlags where
  mempty = UpdateFlags {
    updateVerbosity = mempty
  }
#if !(MIN_VERSION_base(4,11,0))
  mappend a b = UpdateFlags {
    updateVerbosity = combine updateVerbosity
  }
    where combine field = field a `mappend` field b
#endif

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
    -- TODO: parse user's flags as cabal-iinstall does.
    -- Currently I'm lazy to adapt new flag and user:
    --    defaultUpdateFlags
    let updateFlags = commandDefaultFlags CabalInstall.updateCommand
    in CabalInstall.update verbosity updateFlags repoContext

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

#if MIN_VERSION_base(4,9,0)
instance Semigroup MergeFlags where
  a <> b = MergeFlags {
    mergeVerbosity = combine mergeVerbosity
  , mergeCabalFlags = mergeCabalFlags b
  }
    where combine field = field a <> field b
#endif

instance Monoid MergeFlags where
  mempty = MergeFlags {
    mergeVerbosity = mempty
  , mergeCabalFlags = mempty
  }
#if !(MIN_VERSION_base(4,11,0))
  mappend a b = MergeFlags {
    mergeVerbosity = combine mergeVerbosity
  , mergeCabalFlags = mergeCabalFlags b
  }
    where combine field = field a `mappend` field b
#endif

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
-- Utils
-----------------------------------------------------------------------

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
    commandSynopsis = "HackPort is an .ebuild generator from .cabal files with hackage index support",
    commandDescription = Just $ \pname ->
       let
         commands' = commands ++ [commandAddAction helpCommandUI undefined]
         cmdDescs = getNormalCommandDescriptions commands'
         maxlen    = maximum $ [length name | (name, _) <- cmdDescs]
         align str = str ++ replicate (maxlen - length str) ' '
       in
          "Commands:\n"
       ++ unlines [ "  " ++ align name ++ "    " ++ descr
                  | (name, descr) <- cmdDescs ]
       ++ "\n"
       ++ "For more information about a command use\n"
       ++ "  " ++ pname ++ " COMMAND --help\n\n"
       ++ "Typical steps for generating ebuilds from hackage packages:\n"
       ++ concat [ "  " ++ pname ++ " " ++ x ++ "\n"
                 | x <- ["update", "merge <package>"]]
       ++ "\n"
       ++ "Advanced usage:\n"
       ++ concat [ "  " ++ pname ++ " " ++ x ++ "\n"
                 | x <- ["update", "make-ebuild <CATEGORY> <ebuild.name>"]],

    commandNotes = Nothing,
    commandUsage = \pname -> "Usage: " ++ pname ++ " [GLOBAL FLAGS] [COMMAND [FLAGS]]\n",
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

commands :: [Command (H.GlobalFlags -> IO ())]
commands =
      [ listCommand `commandAddAction` listAction
      , makeEbuildCommand `commandAddAction` makeEbuildAction
      , statusCommand `commandAddAction` statusAction
      , updateCommand `commandAddAction` updateAction
      , mergeCommand `commandAddAction` mergeAction
      ]

main :: IO ()
main = getArgs >>= mainWorker

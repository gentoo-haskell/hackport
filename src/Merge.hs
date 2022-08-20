{-|
Module      : Merge
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Core functionality of the @merge@ command of @HackPort@.
-}
module Merge
  ( merge
  , mergeGenericPackageDescription
  ) where

import           Control.Monad
import           Control.Exception
import qualified Data.Text as T
import qualified Data.Text.IO as T
import           Data.Function (on)
import qualified Data.Map.Strict as Map
import           Data.Maybe
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Time.Clock as TC

-- cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.Version as Cabal
import qualified Distribution.PackageDescription as Cabal
import qualified Distribution.PackageDescription.PrettyPrint as Cabal (showPackageDescription)
import qualified Distribution.Solver.Types.SourcePackage as CabalInstall
import qualified Distribution.Solver.Types.PackageIndex as CabalInstall

import           Distribution.Pretty (prettyShow)
import           Distribution.Verbosity
import           Distribution.Simple.Utils

-- cabal-install
import           Distribution.Client.IndexUtils ( getSourcePackages )
import qualified Distribution.Client.GlobalFlags as CabalInstall
import           Distribution.Client.Types
-- others
import           Control.Parallel.Strategies
import qualified Data.List.Split as DLS
import           System.Directory ( getCurrentDirectory
                        , setCurrentDirectory
                        , createDirectoryIfMissing
                        , doesFileExist
                        , listDirectory
                        )
import           System.Process
import           System.FilePath ((</>),(<.>))
import           System.Exit

-- hackport
import qualified AnsiColor as A
import qualified Cabal2Ebuild as C2E
import qualified Portage.EBuild as E
import qualified Portage.EMeta as EM
import           Error as E

import qualified Portage.Cabal as Portage
import qualified Portage.PackageId as Portage
import qualified Portage.Version as Portage
import qualified Portage.Metadata as Portage
import qualified Portage.Overlay as Overlay
import qualified Portage.Resolve as Portage
import qualified Portage.Dependency as Portage
import qualified Portage.Use as Portage

import qualified Portage.GHCCore as GHCCore

import qualified Merge.Dependencies as Merge
import qualified Merge.Utils        as Merge

{-
Requested features:
  * Add files to git?
-}

-- | Call @diff@ between two ebuilds.
diffEbuilds :: FilePath -> Portage.PackageId -> Portage.PackageId -> IO ()
diffEbuilds fp a b = do _ <- system $ "diff -u --color=auto "
                             ++ fp </> Portage.packageIdToFilePath a ++ " "
                             ++ fp </> Portage.packageIdToFilePath b
                        exitSuccess

-- | Given a list of available packages, and maybe a preferred version,
-- return the available package with that version. Latest version is chosen
-- if no preference.
resolveVersion :: [UnresolvedSourcePackage] -> Maybe Cabal.Version -> Maybe UnresolvedSourcePackage
resolveVersion avails Nothing = Just $ L.maximumBy (comparing (Cabal.pkgVersion . CabalInstall.srcpkgPackageId)) avails
resolveVersion avails (Just ver) = listToMaybe (filter match avails)
  where
    match avail = ver == Cabal.pkgVersion (CabalInstall.srcpkgPackageId avail)

-- | This function is executed by the @merge@ command of @HackPort@.
-- Its functionality is as follows:
--
-- 1. Feed user input to 'readPackageString'
-- 2. Look for a matching package on the @hackage@ database
-- 3. Run 'mergeGenericPackageDescription' with the supplied information
-- 4. Generate a coloured diff between the old and new ebuilds.
--
-- Various information is printed in between these steps depending on the
-- 'Verbosity'.
merge :: Verbosity -> CabalInstall.RepoContext -> String -> FilePath -> Maybe String -> IO ()
merge verbosity repoContext packageString overlayPath users_cabal_flags = do
  (m_category, user_pName, m_version) <-
    case Merge.readPackageString packageString of
      Left err -> throw err
      Right (c,p,m_v) ->
        case m_v of
          Nothing -> return (c,p,Nothing)
          Just v -> case Portage.toCabalVersion v of
                      Nothing -> throw (ArgumentError "illegal version")
                      Just ver -> return (c,p,Just ver)

  debug verbosity $ "Category: " ++ show m_category
  debug verbosity $ "Package: " ++ show user_pName
  debug verbosity $ "Version: " ++ show m_version

  let user_pname_str = Cabal.unPackageName user_pName

  overlay <- Overlay.loadLazy overlayPath
  -- portage_path <- Host.portage_dir `fmap` Host.getInfo
  -- portage <- Overlay.loadLazy portage_path
  index <- fmap packageIndex $ getSourcePackages verbosity repoContext

  -- find all packages that maches the user specified package name
  availablePkgs <-
    case map snd (CabalInstall.searchByName index user_pname_str) of
      [] -> throw (PackageNotFound user_pname_str)
      [pkg] -> return pkg
      pkgs  -> do let cabal_pkg_to_pn pkg = Cabal.unPackageName $ Cabal.pkgName (CabalInstall.srcpkgPackageId pkg)
                      names      = map (cabal_pkg_to_pn . L.head) pkgs
                  notice verbosity $ "Ambiguous names: " ++ L.intercalate ", " names
                  forM_ pkgs $ \ps ->
                      do let p_name = (cabal_pkg_to_pn . L.head) ps
                         notice verbosity $ p_name ++ ": " ++ (L.intercalate ", " $ map (prettyShow . Cabal.pkgVersion . CabalInstall.srcpkgPackageId) ps)
                  return $ concat pkgs

  -- select a single package taking into account the user specified version
  selectedPkg <-
    case resolveVersion availablePkgs m_version of
      Nothing -> do
        putStrLn "No such version for that package, available versions:"
        forM_ availablePkgs $ \ avail ->
          putStrLn (prettyShow . CabalInstall.srcpkgPackageId $ avail)
        throw (ArgumentError "no such version for that package")
      Just avail -> return avail

  -- print some info
  info verbosity "Selecting package:"
  forM_ availablePkgs $ \ avail -> do
    let match_text | CabalInstall.srcpkgPackageId avail == CabalInstall.srcpkgPackageId selectedPkg = "* "
                   | otherwise = "- "
    info verbosity $ match_text ++ (prettyShow . CabalInstall.srcpkgPackageId $ avail)

  let cabal_pkgId = CabalInstall.srcpkgPackageId selectedPkg
      norm_pkgName = Cabal.packageName (Portage.normalizeCabalPackageId cabal_pkgId)
  cat <- maybe (Portage.resolveCategory verbosity overlay norm_pkgName) return m_category
  mergeGenericPackageDescription verbosity overlayPath cat (CabalInstall.srcpkgDescription selectedPkg) True users_cabal_flags

  -- Maybe generate a diff
  let pkgPath = overlayPath </> (Portage.unCategory cat) </> (Cabal.unPackageName norm_pkgName)
      newPkgId = Portage.fromCabalPackageId cat cabal_pkgId
  pkgDir <- listDirectory pkgPath
  case Merge.getPreviousPackageId pkgDir newPkgId of
    Just validPkg -> do info verbosity "Generating a diff..."
                        diffEbuilds overlayPath validPkg newPkgId
    _ -> info verbosity "Nothing to diff!"

-- used to be FlagAssignment in Cabal but now it's an opaque type
type CabalFlags = [(Cabal.FlagName, Bool)]

-- | Generate an ebuild from a 'Cabal.GenericPackageDescription'.
mergeGenericPackageDescription :: Verbosity -> FilePath -> Portage.Category -> Cabal.GenericPackageDescription -> Bool -> Maybe String -> IO ()
mergeGenericPackageDescription verbosity overlayPath cat pkgGenericDesc fetch users_cabal_flags = do
  overlay <- Overlay.loadLazy overlayPath
  let merged_cabal_pkg_name = Cabal.pkgName (Cabal.package (Cabal.packageDescription pkgGenericDesc))
      merged_PN = Portage.cabal_pn_to_PN merged_cabal_pkg_name
      pkgdir    = overlayPath </> Portage.unCategory cat </> merged_PN
  existing_meta <- EM.findExistingMeta pkgdir
  let requested_cabal_flags = Merge.first_just_of [users_cabal_flags, EM.cabal_flags existing_meta]

      -- accepts things, like: "cabal_flag:iuse_name", "+cabal_flag", "-cabal_flag"
      read_fas :: Maybe String -> (CabalFlags, [(String, String)])
      read_fas Nothing = ([], [])
      read_fas (Just user_fas_s) = (user_fas, user_renames)
          where user_fas = [ (cf, b)
                           | ((cf, _), Just b) <- cn_in_mb
                           ]
                user_renames = [ (cfn, ein)
                               | ((cabal_cfn, ein), Nothing) <- cn_in_mb
                               , let cfn = Cabal.unFlagName cabal_cfn
                               ]
                cn_in_mb = map read_fa $ DLS.splitOn "," user_fas_s
                read_fa :: String -> ((Cabal.FlagName, String), Maybe Bool)
                read_fa [] = error $ "read_fas: empty flag?"
                read_fa (op:flag) =
                    case op of
                        '+'   -> (get_rename flag, Just True)
                        '-'   -> (get_rename flag, Just False)
                        _     -> (get_rename (op:flag), Nothing)
                  where get_rename :: String -> (Cabal.FlagName, String)
                        get_rename s =
                            case DLS.splitOn ":" s of
                                [cabal_flag_name] -> (Cabal.mkFlagName cabal_flag_name, cabal_flag_name)
                                [cabal_flag_name, iuse_name] -> (Cabal.mkFlagName cabal_flag_name, iuse_name)
                                _                 -> error $ "get_rename: too many components" ++ show (s)

      (user_specified_fas, cf_to_iuse_rename) = read_fas requested_cabal_flags

  debug verbosity "searching for minimal suitable ghc version"
  (compiler_info, ghc_packages, pkgDesc0, _flags, pix) <- case GHCCore.minimumGHCVersionToBuildPackage pkgGenericDesc (Cabal.mkFlagAssignment user_specified_fas) of
              Just v  -> return v
              Nothing -> let pn = prettyShow merged_cabal_pkg_name
                             cn = prettyShow cat
                         in error $ unlines [ "mergeGenericPackageDescription: failed to find suitable GHC for " ++ pn
                                            , "  You can try to merge the package manually:"
                                            , "  $ cabal unpack " ++ pn
                                            , "  $ cd " ++ pn ++ "*/"
                                            , "  # fix " ++ pn ++ ".cabal"
                                            , "  $ hackport make-ebuild " ++ cn ++ " " ++ pn ++ ".cabal"
                                            ]

  let (accepted_deps, skipped_deps) = Portage.partition_depends ghc_packages merged_cabal_pkg_name
                                      (Merge.exeAndLibDeps pkgDesc0)

      pkgDesc = Merge.RetroPackageDescription pkgDesc0 accepted_deps
      cabal_flag_descs = Cabal.genPackageFlags pkgGenericDesc
      all_flags = map Cabal.flagName cabal_flag_descs
      make_fas  :: [Cabal.PackageFlag] -> [CabalFlags]
      make_fas  [] = [[]]
      make_fas  (f:rest) = [ (fn, is_enabled) : fas
                           | fas <- make_fas rest
                           , let fn = Cabal.flagName f
                                 users_choice = lookup fn user_specified_fas
                           , is_enabled <- maybe [False, True]
                                                 (\b -> [b])
                                                 users_choice
                           ]
      all_possible_flag_assignments :: [CabalFlags]
      all_possible_flag_assignments = make_fas cabal_flag_descs

      pp_fa :: CabalFlags -> String
      pp_fa fa = L.intercalate ", " [ (if b then '+' else '-') : f
                                    | (cabal_f, b) <- fa
                                    , let f = Cabal.unFlagName cabal_f
                                    ]

      cfn_to_iuse :: String -> String
      cfn_to_iuse cfn =
          case lookup cfn cf_to_iuse_rename of
              Nothing  -> Merge.mangle_iuse cfn
              Just ein -> ein

      -- key idea is to generate all possible list of flags
      deps1 :: [(CabalFlags, Merge.EDep)]
      deps1  = [ ( f `updateFa` Cabal.unFlagAssignment fr
                 , cabal_to_emerge_dep pkgDesc_filtered_bdeps)
               | f <- all_possible_flag_assignments
               , Right (pkgDesc1,fr) <- [GHCCore.finalizePD (Cabal.mkFlagAssignment f)
                                         GHCCore.defaultComponentRequestedSpec
                                         (GHCCore.dependencySatisfiable pix)
                                         GHCCore.platform
                                         compiler_info
                                         []
                                         pkgGenericDesc]
               -- drop circular deps and shipped deps
               , let (ad, _sd) = Portage.partition_depends ghc_packages merged_cabal_pkg_name
                                 (Merge.exeAndLibDeps pkgDesc1)
               -- TODO: drop ghc libraries from tests depends as well
               -- (see deepseq in hackport-0.3.5 as an example)
               , let pkgDesc_filtered_bdeps = Merge.RetroPackageDescription pkgDesc1 ad
               ] `using` parList rdeepseq
          where
            updateFa :: CabalFlags -> CabalFlags -> CabalFlags
            updateFa [] _ = []
            updateFa (x:xs) y = case lookup (fst x) y of
                                  -- TODO: when does this code get triggered?
                                  Nothing ->          x : updateFa xs y
                                  Just y' -> (fst x,y') : updateFa xs y
      -- then remove all flags that can't be changed
      successfully_resolved_flag_assignments = map fst deps1
      common_fa = L.foldl1' L.intersect successfully_resolved_flag_assignments
      common_flags = map fst common_fa
      active_flags = all_flags L.\\ common_flags
      active_flag_descs = filter (\x -> Cabal.flagName x `elem` active_flags) cabal_flag_descs
      irresolvable_flag_assignments = all_possible_flag_assignments L.\\ successfully_resolved_flag_assignments
      -- flags, not guarding any dependency variation, like:
      --     if flag(foo)
      --         ghc-options: -O2
      (irrelevant_flags, deps1') = L.foldl' drop_irrelevant ([], deps1) active_flags
          where drop_irrelevant :: ([Cabal.FlagName], [(CabalFlags, Merge.EDep)]) -> Cabal.FlagName -> ([Cabal.FlagName], [(CabalFlags, Merge.EDep)])
                drop_irrelevant (ifs, ds) f =
                    case fenabled_ds' == fdisabled_ds' of
                        True  -> (f:ifs, fenabled_ds')
                        False -> (  ifs, ds)
                    where (fenabled_ds', fdisabled_ds') = ( L.sort $ map drop_f fenabled_ds
                                                          , L.sort $ map drop_f fdisabled_ds
                                                          )
                          drop_f :: (CabalFlags, Merge.EDep) -> (CabalFlags, Merge.EDep)
                          drop_f (fas, d) = (filter ((f /=) . fst) fas, d)
                          (fenabled_ds, fdisabled_ds) = L.partition is_fe ds
                          is_fe :: (CabalFlags, Merge.EDep) -> Bool
                          is_fe (fas, _d) =
                              case lookup f fas of
                                  Just v  -> v
                                  -- should not happen
                                  Nothing -> error $ unwords [ "ERROR: drop_irrelevant: searched for missing flag"
                                                             , show f
                                                             , "in assignment"
                                                             , show fas
                                                             ]

      -- and finally prettify all deps:
      leave_only_dynamic_fa :: CabalFlags -> CabalFlags
      leave_only_dynamic_fa fa = filter (\(fn, _) -> all (fn /=) irrelevant_flags) $ fa L.\\ common_fa

      -- build roughly balanced complete dependency tree instead of skewed one
      bimerge :: [Merge.EDep] -> Merge.EDep
      bimerge deps = case go deps of
                         []  -> mempty
                         [r] -> r
                         _   -> error "bimerge: something bad happened"
          where go deps' =
                    case deps' of
                        (d1:d2:ds) -> go (mappend d1 d2 : go ds)
                        _          -> deps'

      tdeps :: Merge.EDep
      tdeps = bimerge $ map set_fa_to_ed deps1'

      set_fa_to_ed :: (CabalFlags, Merge.EDep) -> Merge.EDep
      set_fa_to_ed (fa, ed) = ed { Merge.rdep = liftFlags (leave_only_dynamic_fa fa) $ Merge.rdep ed
                                 , Merge.dep  = liftFlags (leave_only_dynamic_fa fa) $ Merge.dep ed
                                 }

      liftFlags :: CabalFlags -> Portage.Dependency -> Portage.Dependency
      liftFlags fs e = let k = foldr (\(y,b) x -> Portage.mkUseDependency (b, Portage.Use . cfn_to_iuse . Cabal.unFlagName $ y) . x)
                                      id fs
                       in k e

      cabal_to_emerge_dep :: Merge.RetroPackageDescription -> Merge.EDep
      cabal_to_emerge_dep cabal_pkg = Merge.resolveDependencies overlay cabal_pkg compiler_info ghc_packages merged_cabal_pkg_name

  -- When there are lots of package flags, computation of every possible flag combination
  -- can take a while (e.g., 12 package flags = 2^12 possible flag combinations).
  -- Warn the user about this if there are at least 12 package flags. 'cabal_flag_descs'
  -- is usually an overestimation since it includes flags that hackport will strip out,
  -- but using it instead of 'active_flag_descs' avoids forcing the very computation we
  -- are trying to warn the user about.
  when (length cabal_flag_descs >= 12) $
    notice verbosity $ "There are up to " ++
    A.bold (show (2^(length cabal_flag_descs) :: Int)) ++
    " possible flag combinations.\n" ++
    A.inColor A.Yellow True A.Default "This may take a while."

  debug verbosity $ "buildDepends pkgDesc0 raw: " ++ Cabal.showPackageDescription pkgDesc0
  debug verbosity $ "buildDepends pkgDesc0: " ++ show (map prettyShow (Merge.exeAndLibDeps pkgDesc0))
  debug verbosity $ "buildDepends pkgDesc:  " ++ show (map prettyShow (Merge.buildDepends pkgDesc))

  notice verbosity $ "Accepted depends: " ++ show (map prettyShow accepted_deps)
  notice verbosity $ "Skipped  depends: " ++ show (map prettyShow skipped_deps)
  notice verbosity $ "Dead flags: " ++ show (map pp_fa irresolvable_flag_assignments)
  notice verbosity $ "Dropped  flags: " ++ show (map (Cabal.unFlagName.fst) common_fa)
  notice verbosity $ "Active flags: " ++ show (map Cabal.unFlagName active_flags)
  notice verbosity $ "Irrelevant flags: " ++ show (map Cabal.unFlagName irrelevant_flags)
  -- mapM_ print tdeps

  forM_ ghc_packages $
      \name -> info verbosity $ "Excluded packages (comes with ghc): " ++ Cabal.unPackageName name

  let pp_fn (cabal_fn, yesno) = b yesno ++ Cabal.unFlagName cabal_fn
          where b True  = ""
                b False = "-"

      -- appends 's' to each line except the last one
      --  handy to build multiline shell expressions
      icalate _s []     = []
      icalate _s [x]    = [x]
      icalate  s (x:xs) = (x ++ s) : icalate s xs

      build_configure_call :: [String] -> [String]
      build_configure_call [] = []
      build_configure_call conf_args = icalate " \\" $
                                           "haskell-cabal_src_configure" :
                                           map ('\t':) conf_args

      -- returns list USE-parameters to './setup configure'
      selected_flags :: ([Cabal.FlagName], CabalFlags) -> [String]
      selected_flags ([], []) = []
      selected_flags (active_fns, users_fas) = map snd (L.sortBy (compare `on` fst) flag_pairs)
          where flag_pairs :: [(String, String)]
                flag_pairs = active_pairs ++ users_pairs
                active_pairs = map (\fn -> (fn,                    "$(cabal_flag " ++ cfn_to_iuse fn ++ " " ++ fn ++ ")")) $ map Cabal.unFlagName active_fns
                users_pairs  = map (\fa -> ((Cabal.unFlagName . fst) fa, "--flag=" ++ pp_fn fa)) users_fas
      to_iuse x = let fn = Cabal.unFlagName $ Cabal.flagName x
                      p  = if Cabal.flagDefault x then "+" else ""
                  in p ++ cfn_to_iuse fn

      ebuild =   (\e -> e { E.depend        =            Merge.dep tdeps} )
               . (\e -> e { E.depend_extra  = S.toList $ Merge.dep_e tdeps } )
               . (\e -> e { E.rdepend       =            Merge.rdep tdeps} )
               . (\e -> e { E.rdepend_extra = S.toList $ Merge.rdep_e tdeps } )
               . (\e -> e { E.src_configure = build_configure_call $
                                                  selected_flags (active_flags, user_specified_fas) } )
               . (\e -> e { E.iuse = E.iuse e ++ map to_iuse active_flag_descs })
               . ( case requested_cabal_flags of
                       Nothing  -> id
                       Just ucf -> (\e -> e { E.used_options  = E.used_options e ++ [("flags", ucf)] }))
               $ C2E.cabal2ebuild cat (Merge.packageDescription pkgDesc)

  let active_flag_descs_renamed =
        (\f -> f { Cabal.flagName = Cabal.mkFlagName . cfn_to_iuse . Cabal.unFlagName
                   . Cabal.flagName $ f }) <$> active_flag_descs
  iuse_flag_descs <- Merge.dropIfUseExpands active_flag_descs_renamed
  mergeEbuild verbosity existing_meta pkgdir ebuild iuse_flag_descs

  when fetch $ do
    let cabal_pkgId = Cabal.packageId (Merge.packageDescription pkgDesc)
        norm_pkgName = Cabal.packageName (Portage.normalizeCabalPackageId cabal_pkgId)
    fetchDigestAndCheck verbosity (overlayPath </> prettyShow cat </> prettyShow norm_pkgName)
      $ Portage.fromCabalPackageId cat cabal_pkgId

-- | Run @ebuild@ and @pkgcheck@ commands in the directory of the
-- newly-generated ebuild.
--
-- This will ensure well-formed ebuilds and @metadata.xml@, and will update (if possible)
-- the @Manifest@ file.
fetchDigestAndCheck :: Verbosity
                    -> FilePath -- ^ directory of ebuild
                    -> Portage.PackageId -- ^ newest ebuild
                    -> IO ()
fetchDigestAndCheck verbosity ebuildDir pkgId =
  let ebuild = prettyShow (Portage.cabalPkgName . Portage.packageId $ pkgId)
               ++ "-" ++ prettyShow (Portage.pkgVersion pkgId) <.> "ebuild"
  in withWorkingDirectory ebuildDir $ do
    notice verbosity "Recalculating digests..."
    emEx <- system $ "ebuild " ++ ebuild ++ " manifest > /dev/null 2>&1"
    when (emEx /= ExitSuccess) $
      notice verbosity "ebuild manifest failed horribly. Do something about it!"

    notice verbosity $ "Running " ++ A.bold "pkgcheck scan..."

    (psEx,psOut,_) <- readCreateProcessWithExitCode (shell "pkgcheck scan --color True") ""
    
    when (psEx /= ExitSuccess) $ -- this should never be true, even with QA issues.
      notice verbosity $ A.inColor A.Red True A.Default "pkgcheck scan failed."
    notice verbosity psOut

    return ()

withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory newDir action = do
  oldDir <- getCurrentDirectory
  bracket
    (setCurrentDirectory newDir)
    (\_ -> setCurrentDirectory oldDir)
    (\_ -> action)

-- | Write the ebuild (and sometimes a new @metadata.xml@) to its directory.
mergeEbuild :: Verbosity -> EM.EMeta -> FilePath -> E.EBuild -> [Cabal.PackageFlag] -> IO ()
mergeEbuild verbosity existing_meta pkgdir ebuild flags = do
  let edir = pkgdir
      elocal = E.name ebuild ++"-"++ E.version ebuild <.> "ebuild"
      epath = edir </> elocal
      emeta = "metadata.xml"
      mpath = edir </> emeta
  yet_meta <- doesFileExist mpath
  -- If there is an existing @metadata.xml@, read it in as a 'T.Text'.
  -- Otherwise return 'T.empty'. We only use this once more to directly
  -- compare to @default_meta@ before writing it.
  current_meta <- if yet_meta
                  then T.readFile mpath
                  else return T.empty
  -- Either create an object of the 'Portage.Metadata' type from a valid @current_meta@,
  -- or supply a default minimal metadata object. Note the difference to @current_meta@:
  -- @current_meta@ is of type 'T.Text', @current_meta'@ is of type 'Portage.Metadata'.
  let current_meta' = fromMaybe Portage.makeMinimalMetadata
                      (Portage.pureMetadataFromFile current_meta)
      -- Create the @metadata.xml@ string, adding new USE flags (if any) to those of
      -- the existing @metadata.xml@. If an existing flag has a new and old description,
      -- the new one takes precedence.
      default_meta = Portage.makeDefaultMetadata
                     $ Merge.metaFlags flags `Map.union`
                     Portage.metadataUseFlags current_meta'
      -- Create a 'Map.Map' of USE flags with updated descriptions.
      new_flags = Map.differenceWith (\new old -> if (new /= old)
                                                  then Just $ old ++ A.bold (" -> " ++ new)
                                                  else Nothing)
                  (Merge.metaFlags flags)
                  $ Portage.metadataUseFlags current_meta'

  createDirectoryIfMissing True edir
  now <- TC.getCurrentTime

  let (existing_keywords, existing_license) = (EM.keywords existing_meta, EM.license existing_meta)
      new_keywords = maybe (E.keywords ebuild) (map Merge.to_unstable) existing_keywords
      new_license  = either (\err -> maybe (Left err)
                                           Right
                                           existing_license)
                            Right
                            (E.license ebuild)
      ebuild'      = ebuild { E.keywords = new_keywords
                            , E.license = new_license
                            }
      s_ebuild'    = E.showEBuild now ebuild'

  notice verbosity $ "Current keywords: " ++ show existing_keywords ++ " -> " ++ show new_keywords
  notice verbosity $ "Current license:  " ++ show existing_license ++ " -> " ++ show new_license

  notice verbosity $ "Writing " ++ elocal
  length s_ebuild' `seq` T.writeFile epath (T.pack s_ebuild')

  when (current_meta /= default_meta) $ do
    when (current_meta /= T.empty) $ do
      notice verbosity $ A.bold $ "Default and current " ++ emeta ++ " differ."
      if (new_flags /= Map.empty)
        then notice verbosity $ "New or updated USE flags:\n" ++
             (unlines $ Portage.prettyPrintFlagsHuman new_flags)
        else notice verbosity "No new USE flags."

    notice verbosity $ "Writing " ++ emeta
    T.writeFile mpath default_meta

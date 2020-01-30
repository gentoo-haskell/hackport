module Merge
  ( merge
  , mergeGenericPackageDescription
  ) where

import Control.Monad
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.ByteString.Builder as BL (stringUtf8, toLazyByteString)
import Data.Function (on)
import Data.Maybe
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

import Distribution.Pretty (prettyShow)
import Distribution.Verbosity
import Distribution.Simple.Utils

-- cabal-install
import Distribution.Client.IndexUtils ( getSourcePackages )
import qualified Distribution.Client.GlobalFlags as CabalInstall
import Distribution.Client.Types

-- others
import qualified Data.List.Split as DLS
import System.Directory ( getCurrentDirectory
                        , setCurrentDirectory
                        , createDirectoryIfMissing
                        , doesFileExist
                        , listDirectory
                        )
import System.Process (system)
import System.FilePath ((</>))
import qualified System.FilePath as SF
import System.Exit

import qualified Cabal2Ebuild as C2E
import qualified Portage.EBuild as E
import qualified Portage.EMeta as EM
import Error as E

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

(<.>) :: String -> String -> String
a <.> b = a ++ '.':b

{-
Requested features:
  * Add files to git?
-}

readPackageString :: [String]
                  -> Either HackPortError ( Maybe Portage.Category
                                          , Cabal.PackageName
                                          , Maybe Portage.Version
                                          )
readPackageString args = do
  packageString <-
    case args of
      [] -> Left (ArgumentError "Need an argument, [category/]package[-version]")
      [pkg] -> return pkg
      _ -> Left (ArgumentError ("Too many arguments: " ++ unwords args))
  case Portage.parseFriendlyPackage packageString of
    Right v@(_,_,Nothing) -> return v
    -- we only allow versions we can convert into cabal versions
    Right v@(_,_,Just (Portage.Version _ Nothing [] 0)) -> return v
    Left e -> Left $ ArgumentError $ "Could not parse [category/]package[-version]: "
              ++ packageString ++ "\nParsec error: " ++ e
    _ -> Left $ ArgumentError $ "Could not parse [category/]package[-version]: "
         ++ packageString

-- | Given a list of available packages, and maybe a preferred version,
-- return the available package with that version. Latest version is chosen
-- if no preference.
resolveVersion :: [UnresolvedSourcePackage] -> Maybe Cabal.Version -> Maybe UnresolvedSourcePackage
resolveVersion avails Nothing = Just $ L.maximumBy (comparing (Cabal.pkgVersion . CabalInstall.packageInfoId)) avails
resolveVersion avails (Just ver) = listToMaybe (filter match avails)
  where
    match avail = ver == Cabal.pkgVersion (CabalInstall.packageInfoId avail)

merge :: Verbosity -> CabalInstall.RepoContext -> [String] -> FilePath -> Maybe String -> IO ()
merge verbosity repoContext args overlayPath users_cabal_flags = do
  (m_category, user_pName, m_version) <-
    case readPackageString args of
      Left err -> throwEx err
      Right (c,p,m_v) ->
        case m_v of
          Nothing -> return (c,p,Nothing)
          Just v -> case Portage.toCabalVersion v of
                      Nothing -> throwEx (ArgumentError "illegal version")
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
      [] -> throwEx (PackageNotFound user_pname_str)
      [pkg] -> return pkg
      pkgs  -> do let cabal_pkg_to_pn pkg = Cabal.unPackageName $ Cabal.pkgName (CabalInstall.packageInfoId pkg)
                      names      = map (cabal_pkg_to_pn . L.head) pkgs
                  notice verbosity $ "Ambiguous names: " ++ L.intercalate ", " names
                  forM_ pkgs $ \ps ->
                      do let p_name = (cabal_pkg_to_pn . L.head) ps
                         notice verbosity $ p_name ++ ": " ++ (L.intercalate ", " $ map (prettyShow . Cabal.pkgVersion . CabalInstall.packageInfoId) ps)
                  return $ concat pkgs

  -- select a single package taking into account the user specified version
  selectedPkg <-
    case resolveVersion availablePkgs m_version of
      Nothing -> do
        putStrLn "No such version for that package, available versions:"
        forM_ availablePkgs $ \ avail ->
          putStrLn (prettyShow . CabalInstall.packageInfoId $ avail)
        throwEx (ArgumentError "no such version for that package")
      Just avail -> return avail

  -- print some info
  info verbosity "Selecting package:"
  forM_ availablePkgs $ \ avail -> do
    let match_text | CabalInstall.packageInfoId avail == CabalInstall.packageInfoId selectedPkg = "* "
                   | otherwise = "- "
    info verbosity $ match_text ++ (prettyShow . CabalInstall.packageInfoId $ avail)

  let cabal_pkgId = CabalInstall.packageInfoId selectedPkg
      norm_pkgName = Cabal.packageName (Portage.normalizeCabalPackageId cabal_pkgId)
  cat <- maybe (Portage.resolveCategory verbosity overlay norm_pkgName) return m_category
  mergeGenericPackageDescription verbosity overlayPath cat (CabalInstall.packageDescription selectedPkg) True users_cabal_flags

  -- Maybe generate a diff
  let pkgPath = overlayPath </> (Portage.unCategory cat) </> (Cabal.unPackageName norm_pkgName)
      newPkgId = Portage.fromCabalPackageId cat cabal_pkgId
  pkgDir <- listDirectory pkgPath
  case getPreviousPackageId pkgDir newPkgId of
    Just validPkg -> do info verbosity "Generating a diff..."
                        diffEbuilds overlayPath validPkg newPkgId
    _ -> info verbosity "Nothing to diff!"
  
-- | Call diff between two ebuilds.
diffEbuilds :: FilePath -> Portage.PackageId -> Portage.PackageId -> IO ()
diffEbuilds fp a b = do _ <- system $ "diff -u --color=auto "
                             ++ fp </> Portage.packageIdToFilePath a ++ " "
                             ++ fp </> Portage.packageIdToFilePath b
                        exitSuccess

-- | Maybe return a PackageId of the next highest version for a given
--   package, relative to the provided PackageId.
getPreviousPackageId :: [FilePath] -- ^ list of ebuilds for given package
                     -> Portage.PackageId -- ^ new PackageId
                     -> Maybe Portage.PackageId -- ^ maybe PackageId of previous version
getPreviousPackageId pkgDir newPkgId = do
  let pkgIds = reverse 
               . L.sortOn (Portage.pkgVersion)
               . filter (<newPkgId)
               $ mapMaybe (Portage.filePathToPackageId (Portage.category . Portage.packageId $ newPkgId))
               $ SF.dropExtension <$> filter (\fp -> SF.takeExtension fp == ".ebuild") pkgDir
  case pkgIds of
    x:_ -> Just x
    _ -> Nothing

first_just_of :: [Maybe a] -> Maybe a
first_just_of = msum

-- Gentoo allows underscore ('_') names in IUSE only for
-- USE_EXPAND values. If it's not a user-specified rename mangle
-- it into a hyphen ('-').
mangle_iuse :: String -> String
mangle_iuse = drop_prefix . map f
  where f '_' = '-'
        f c   = c

-- | Remove "with" or "use" prefixes from flag names.
drop_prefix :: String -> String
drop_prefix = \x ->
  case splitAt 5 x of
    ("with_", b) -> b
    ("with-", b) -> b
    _ -> case splitAt 4 x of
           ("use_", b) -> b
           ("use-", b) -> b
           _ -> x

-- used to be FlagAssignment in Cabal but now it's an opaque type
type CabalFlags = [(Cabal.FlagName, Bool)]

mergeGenericPackageDescription :: Verbosity -> FilePath -> Portage.Category -> Cabal.GenericPackageDescription -> Bool -> Maybe String -> IO ()
mergeGenericPackageDescription verbosity overlayPath cat pkgGenericDesc fetch users_cabal_flags = do
  overlay <- Overlay.loadLazy overlayPath
  let merged_cabal_pkg_name = Cabal.pkgName (Cabal.package (Cabal.packageDescription pkgGenericDesc))
      merged_PN = Portage.cabal_pn_to_PN merged_cabal_pkg_name
      pkgdir    = overlayPath </> Portage.unCategory cat </> merged_PN
  existing_meta <- EM.findExistingMeta pkgdir
  let requested_cabal_flags = first_just_of [users_cabal_flags, EM.cabal_flags existing_meta]

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
      make_fas  :: [Cabal.Flag] -> [CabalFlags]
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
              Nothing  -> mangle_iuse cfn
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
               ]
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

  mergeEbuild verbosity existing_meta pkgdir ebuild active_flag_descs
  when fetch $ do
    let cabal_pkgId = Cabal.packageId (Merge.packageDescription pkgDesc)
        norm_pkgName = Cabal.packageName (Portage.normalizeCabalPackageId cabal_pkgId)
    fetchDigestAndCheck verbosity (overlayPath </> prettyShow cat </> prettyShow norm_pkgName)

fetchDigestAndCheck :: Verbosity
                    -> FilePath -- ^ directory of ebuild
                    -> IO ()
fetchDigestAndCheck verbosity ebuildDir =
  withWorkingDirectory ebuildDir $ do
     notice verbosity "Recalculating digests (repoman manifest)..."
     rm <- system "repoman manifest"
     when (rm /= ExitSuccess) $
         notice verbosity "repoman manifest failed horribly. Do something about it!"
     rf <- system "repoman full --include-dev"
     when (rf /= ExitSuccess) $
         notice verbosity "repoman full --include-dev found an error. Do something about it!"
     return ()

withWorkingDirectory :: FilePath -> IO a -> IO a
withWorkingDirectory newDir action = do
  oldDir <- getCurrentDirectory
  bracket
    (setCurrentDirectory newDir)
    (\_ -> setCurrentDirectory oldDir)
    (\_ -> action)

-- "amd64" -> "~amd64"
to_unstable :: String -> String
to_unstable kw =
    case kw of
        '~':_ -> kw
        '-':_ -> kw
        _     -> '~':kw

-- | Generate a list of tuples containing Cabal flag names and descriptions
metaFlags :: [Cabal.Flag] -> [(String, String)]
metaFlags flags = zip (mangle_iuse . Cabal.unFlagName . Cabal.flagName <$> flags) (Cabal.flagDescription <$> flags)

mergeEbuild :: Verbosity -> EM.EMeta -> FilePath -> E.EBuild -> [Cabal.Flag] -> IO ()
mergeEbuild verbosity existing_meta pkgdir ebuild flags = do
  let edir = pkgdir
      elocal = E.name ebuild ++"-"++ E.version ebuild <.> "ebuild"
      epath = edir </> elocal
      emeta = "metadata.xml"
      mpath = edir </> emeta
      default_meta = BL.toLazyByteString . BL.stringUtf8
                     $ Portage.makeDefaultMetadata (E.long_desc ebuild)
                     $ metaFlags flags
  createDirectoryIfMissing True edir
  now <- TC.getCurrentTime

  let (existing_keywords, existing_license, existing_description) = (EM.keywords existing_meta, EM.license existing_meta, EM.description existing_meta)
      new_keywords = maybe (E.keywords ebuild) (map to_unstable) existing_keywords
      new_license  = either (\err -> maybe (Left err)
                                           Right
                                           existing_license)
                            Right
                            (E.license ebuild)
      new_description = maybe (E.description ebuild) id existing_description
      ebuild'      = ebuild { E.keywords = new_keywords
                            , E.license = new_license
                            , E.description = new_description
                            }
      s_ebuild'    = E.showEBuild now ebuild'

  notice verbosity $ "Current keywords: " ++ show existing_keywords ++ " -> " ++ show new_keywords
  notice verbosity $ "Current license:  " ++ show existing_license ++ " -> " ++ show new_license

  notice verbosity $ "Writing " ++ elocal
  length s_ebuild' `seq` BL.writeFile epath (BL.pack s_ebuild')

  yet_meta <- doesFileExist mpath
  if not yet_meta -- TODO: add --force-meta-rewrite to opts
      then do notice verbosity $ "Writing " ++ emeta
              BL.writeFile mpath default_meta
      else do current_meta <- BL.readFile mpath
              when (current_meta /= default_meta) $
                  notice verbosity $ "Default and current " ++ emeta ++ " differ."

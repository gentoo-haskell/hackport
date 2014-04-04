module Merge
  ( merge
  , mergeGenericPackageDescription
  ) where

import Control.Arrow (first, second)
import Control.Monad.Error
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BL
import qualified Data.Map.Strict as M
import Data.Function (on)
import Data.Maybe
import Data.Monoid
import qualified Data.List as L
import qualified Data.Set as S
import qualified Data.Time.Clock as TC
import Data.Version

-- cabal
import qualified Distribution.Package as Cabal
import qualified Distribution.Version as Cabal
import qualified Distribution.PackageDescription as Cabal ( PackageDescription(..)
                                       , Flag(..)
                                       , FlagAssignment
                                       , FlagName(..)
                                       , GenericPackageDescription(..)
                                       )
import qualified Distribution.PackageDescription.Parse as Cabal (showPackageDescription)

import Distribution.Text (display)
import Distribution.Verbosity
import Distribution.Simple.Utils

-- cabal-install
import Distribution.Client.IndexUtils ( getSourcePackages )
import qualified Distribution.Client.PackageIndex as Index
import Distribution.Client.Types

-- others
import System.Directory ( getCurrentDirectory
                        , setCurrentDirectory
                        , createDirectoryIfMissing
                        , doesFileExist
                        )
import System.Cmd (system)
import System.FilePath ((</>))
import System.Exit

import qualified Cabal2Ebuild as C2E
import qualified Portage.EBuild as E
import qualified Portage.EMeta as EM
import Error as E

import Network.URI

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

import qualified Util as U

(<.>) :: String -> String -> String
a <.> b = a ++ '.':b

{-
Requested features:
  * Add files to git?
  * Print diff with the next latest version?
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
    Just v@(_,_,Nothing) -> return v
    -- we only allow versions we can convert into cabal versions
    Just v@(_,_,Just (Portage.Version _ Nothing [] 0)) -> return v
    _ -> Left (ArgumentError ("Could not parse [category/]package[-version]: " ++ packageString))



-- | Given a list of available packages, and maybe a preferred version,
-- return the available package with that version. Latest version is chosen
-- if no preference.
resolveVersion :: [SourcePackage] -> Maybe Cabal.Version -> Maybe SourcePackage
resolveVersion avails Nothing = Just $ L.maximumBy (comparing packageInfoId) avails
resolveVersion avails (Just ver) = listToMaybe (filter match avails)
  where
    match avail = ver == Cabal.pkgVersion (packageInfoId avail)

merge :: Verbosity -> Repo -> URI -> [String] -> FilePath -> Maybe String -> IO ()
merge verbosity repo _serverURI args overlayPath users_cabal_flags = do
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

  let (Cabal.PackageName user_pname_str) = user_pName

  overlay <- Overlay.loadLazy overlayPath
  -- portage_path <- Host.portage_dir `fmap` Host.getInfo
  -- portage <- Overlay.loadLazy portage_path
  index <- fmap packageIndex $ getSourcePackages verbosity [ repo ]

  -- find all packages that maches the user specified package name
  availablePkgs <-
    case map snd (Index.searchByName index user_pname_str) of
      [] -> throwEx (PackageNotFound user_pname_str)
      [pkg] -> return pkg
      pkgs  -> do let cabal_pkg_to_pn pkg =
                          case Cabal.pkgName (packageInfoId pkg) of
                              Cabal.PackageName pn -> pn
                      names      = map (cabal_pkg_to_pn . L.head) pkgs
                  notice verbosity $ "Ambiguous names: " ++ L.intercalate ", " names
                  forM_ pkgs $ \ps ->
                      do let p_name = (cabal_pkg_to_pn . L.head) ps
                         notice verbosity $ p_name ++ ": " ++ (L.intercalate ", " $ map (showVersion . Cabal.pkgVersion . packageInfoId) ps)
                  return $ concat pkgs

  -- select a single package taking into account the user specified version
  selectedPkg <-
    case resolveVersion availablePkgs m_version of
      Nothing -> do
        putStrLn "No such version for that package, available versions:"
        forM_ availablePkgs $ \ avail ->
          putStrLn (display . packageInfoId $ avail)
        throwEx (ArgumentError "no such version for that package")
      Just avail -> return avail

  -- print some info
  info verbosity "Selecting package:"
  forM_ availablePkgs $ \ avail -> do
    let match_text | packageInfoId avail == packageInfoId selectedPkg = "* "
                   | otherwise = "- "
    info verbosity $ match_text ++ (display . packageInfoId $ avail)

  let cabal_pkgId = packageInfoId selectedPkg
      norm_pkgName = Cabal.packageName (Portage.normalizeCabalPackageId cabal_pkgId)
  cat <- maybe (Portage.resolveCategory verbosity overlay norm_pkgName) return m_category
  mergeGenericPackageDescription verbosity overlayPath cat (packageDescription selectedPkg) True users_cabal_flags

first_just_of :: [Maybe a] -> Maybe a
first_just_of = msum

mergeGenericPackageDescription :: Verbosity -> FilePath -> Portage.Category -> Cabal.GenericPackageDescription -> Bool -> Maybe String -> IO ()
mergeGenericPackageDescription verbosity overlayPath cat pkgGenericDesc fetch users_cabal_flags = do
  overlay <- Overlay.loadLazy overlayPath
  let merged_cabal_pkg_name = Cabal.pkgName (Cabal.package (Cabal.packageDescription pkgGenericDesc))
      merged_PN = Portage.cabal_pn_to_PN merged_cabal_pkg_name
      pkgdir    = overlayPath </> Portage.unCategory cat </> merged_PN
  existing_meta <- EM.findExistingMeta pkgdir
  let requested_cabal_flags = first_just_of [users_cabal_flags, EM.cabal_flags existing_meta]

      -- accepts things, like: "cabal_flag:iuse_name", "+cabal_flag", "-cabal_flag"
      read_fas :: Maybe String -> (Cabal.FlagAssignment, [(String, String)])
      read_fas Nothing = ([], [])
      read_fas (Just user_fas_s) = (user_fas, user_renames)
          where user_fas = [ (cf, b)
                           | ((cf, _), Just b) <- cn_in_mb
                           ]
                user_renames = [ (cfn, ein)
                               | ((Cabal.FlagName cfn, ein), Nothing) <- cn_in_mb
                               ]
                cn_in_mb = map read_fa $ U.split (== ',') user_fas_s
                read_fa :: String -> ((Cabal.FlagName, String), Maybe Bool)
                read_fa [] = error $ "read_fas: empty flag?"
                read_fa (op:flag) =
                    case op of
                        '+'   -> (get_rename flag, Just True)
                        '-'   -> (get_rename flag, Just False)
                        _     -> (get_rename (op:flag), Nothing)
                  where get_rename :: String -> (Cabal.FlagName, String)
                        get_rename s =
                            case U.split (== ':') s of
                                [cabal_flag_name] -> (Cabal.FlagName cabal_flag_name, cabal_flag_name)
                                [cabal_flag_name, iuse_name] -> (Cabal.FlagName cabal_flag_name, iuse_name)
                                _                 -> error $ "get_rename: too many components" ++ show (s)

      (user_specified_fas, cf_to_iuse_rename) = read_fas requested_cabal_flags

  debug verbosity "searching for minimal suitable ghc version"
  (compilerId, ghc_packages, pkgDesc0, _flags, pix) <- case GHCCore.minimumGHCVersionToBuildPackage pkgGenericDesc user_specified_fas of
              Just v  -> return v
              Nothing -> let pn = display merged_cabal_pkg_name
                             cn = display cat
                         in error $ unlines [ "mergeGenericPackageDescription: failed to find suitable GHC for " ++ pn
                                            , "  You can try to merge the package manually:"
                                            , "  $ cabal unpack " ++ pn
                                            , "  $ cd " ++ pn ++ "*/"
                                            , "  # fix " ++ pn ++ ".cabal"
                                            , "  $ hackport make-ebuild " ++ cn ++ " " ++ pn ++ ".cabal"
                                            ]

  let (accepted_deps, skipped_deps) = Portage.partition_depends ghc_packages merged_cabal_pkg_name (Cabal.buildDepends pkgDesc0)
      pkgDesc = pkgDesc0 { Cabal.buildDepends = accepted_deps }
      cabal_flag_descs = Cabal.genPackageFlags pkgGenericDesc
      all_flags = map Cabal.flagName cabal_flag_descs
      make_fas  :: [Cabal.Flag] -> [Cabal.FlagAssignment]
      make_fas  [] = [[]]
      make_fas  (f:rest) = [ (fn, is_enabled) : fas
                           | fas <- make_fas rest
                           , let fn = Cabal.flagName f
                                 users_choice = lookup fn user_specified_fas
                           , is_enabled <- maybe [False, True]
                                                 (\b -> [b])
                                                 users_choice
                           ]
      all_possible_flag_assignments :: [Cabal.FlagAssignment]
      all_possible_flag_assignments = make_fas cabal_flag_descs

      pp_fa :: Cabal.FlagAssignment -> String
      pp_fa fa = L.intercalate ", " [ (if b then '+' else '-') : f
                                    | (Cabal.FlagName f, b) <- fa
                                    ]


      cfn_to_iuse :: String -> String
      cfn_to_iuse cfn =
          case lookup cfn cf_to_iuse_rename of
              Nothing  -> cfn
              Just ein -> ein

      -- key idea is to generate all possible list of flags
      deps1 :: [(Cabal.FlagAssignment, Merge.EDep)]
      deps1  = [ (f `updateFa` fr, cabal_to_emerge_dep pkgDesc_filtered_bdeps)
               | f <- all_possible_flag_assignments
               , Right (pkgDesc1,fr) <- [GHCCore.finalizePackageDescription f
                                                                  (GHCCore.dependencySatisfiable pix)
                                                                  GHCCore.platform
                                                                  compilerId
                                                                  []
                                                                  pkgGenericDesc]
               -- drop circular deps and shipped deps
               , let (ad, _sd) = Portage.partition_depends ghc_packages merged_cabal_pkg_name (Cabal.buildDepends pkgDesc1)
               -- TODO: drop ghc libraries from tests depends as well
               -- (see deepseq in hackport-0.3.5 as an example)
               , let pkgDesc_filtered_bdeps = pkgDesc1 { Cabal.buildDepends = ad }
               ]
          where
            updateFa :: Cabal.FlagAssignment -> Cabal.FlagAssignment -> Cabal.FlagAssignment
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
      -- and finally prettify all deps:
      leave_only_dynamic_fa :: Cabal.FlagAssignment -> Cabal.FlagAssignment
      leave_only_dynamic_fa fa = fa L.\\ common_fa

      optimize_fa_depends :: [([(Cabal.FlagName, Bool)], [Portage.Dependency])] -> [Portage.Dependency]
      optimize_fa_depends deps = Portage.sortDeps
                               . simplify
                               . map ( (\fdep -> (fdep, []))
                                     . first leave_only_dynamic_fa) $ deps

      tdeps :: Merge.EDep
      tdeps = (L.foldl' (\x y -> x `mappend` snd y) mempty deps1){
            Merge.dep  = optimize_fa_depends $ map (second Merge.dep) deps1
          , Merge.rdep = optimize_fa_depends $ map (second Merge.rdep) deps1
          }

      pop_common_deps :: [(FaDep,[FaDep])] -> (FaDep,[FaDep])
      pop_common_deps xs =
           case pop_from_pairs xs of
                 []  -> error "impossible"
                 [x] -> x
                 r   -> pop_common_deps r
          where
            pop_from_pairs :: [(FaDep,[FaDep])] -> [(FaDep,[FaDep])]
            pop_from_pairs [] = []
            pop_from_pairs [y] = [y]
            pop_from_pairs (y1:y2:rest) = y1 `pop_from_pair` y2 : pop_from_pairs rest

            pop_from_pair :: (FaDep,[FaDep]) -> (FaDep,[FaDep]) -> (FaDep,[FaDep])
            pop_from_pair ((lfa, ld), lx) ((rfa, rd), rx) = ((fa, d), x)
                where fa = lfa `L.intersect` rfa
                      d  = ld `L.intersect` rd
                      x  = (lfa, ld L.\\ d)
                         : (rfa, rd L.\\ d)
                         : lx ++ rx

      simplify :: [(FaDep,[FaDep])] -> [Portage.Dependency]
      simplify fdephs =
        let -- extract common part of the depends
            -- filtering out empty groups
            ((common_fas, common_fdeps), all_fdeps) = second (filter (not . null . snd)) $ pop_common_deps fdephs
            -- apply assumption of 'fdep' on other depends
            -- Handle at least:
            --  1. redundant-USE cancelation
            --    a? b? c? ( x ) a? ( x ) => a? ( x )
            --  2. one-USE irrelevance
            --    a? b? c? d? ( x ) a? b? !c? d? ( x ) => a? b? d? ( x )
            -- Ideally this thing should be multipass
            mergeD :: (Cabal.FlagAssignment, Portage.Dependency)
                   -> [(Cabal.FlagAssignment, Portage.Dependency)]
                   -> [(Cabal.FlagAssignment, Portage.Dependency)]
            mergeD fdep [] = [fdep]
            mergeD lfdep@(lfa, ld) (rfdep@(rfa, rd):rest) =
                case (ld == rd, slfa `S.intersection` srfa) of
                    -- [1]
                    (True, ifa) | ifa == slfa || ifa == srfa
                              -> mergeD (S.toList ifa, ld) rest
                    -- [2]
                    (True, ifa) | case (S.toList (slfa S.\\ ifa), S.toList (srfa S.\\ ifa)) of
                                      ([(lfn, lfv)], [(rfn, rfv)])
                                          -> lfn == rfn && lfv == not rfv
                                      _   -> False
                              -> mergeD (S.toList ifa, ld) rest
                    -- otherwise
                    _         -> rfdep:mergeD lfdep rest
              where slfa = S.fromList lfa
                    srfa = S.fromList rfa

            sd :: [(Cabal.FlagAssignment, [Portage.Dependency])]
            sd = M.toList $!
                 L.foldl' (\fadeps (fa, new_deps) -> let push_front old_val = Just $!
                                                             case old_val of
                                                                 Nothing -> new_deps:[]
                                                                 Just ds -> new_deps:ds
                                                     in M.alter push_front fa fadeps
                       ) M.empty $ L.foldl' (\fadeps fadep -> fadep `mergeD` fadeps)
                                    []
                                    (concatMap (\(fa, deps) -> map (\one_dep -> (fa, one_dep))
                                                                   deps)
                                               all_fdeps)
            -- filter out splitted packages from common group
            ys = filter (not.null.snd) $ map (second (filter (\d -> d `notElem` concatMap snd sd)
                                                     )) all_fdeps
            -- Now we need to find noniteracting use flags if they are then we
            -- don't need to simplify them more, and output as-is
            simplifyMore :: [(Cabal.FlagAssignment,[Portage.Dependency])] -> [Portage.Dependency]
            simplifyMore [] = []
            simplifyMore fdeps =
                let fa_hist = get_fa_hist fdeps
                    (u,_) = L.maximumBy (compare `on` snd) fa_hist
                    (fdeps_u, fdeps_nu) = hasFlag u `L.partition` fdeps
                in if null fa_hist
                      then concatMap (\(a, b) -> liftFlags a b) fdeps
                      else liftFlags [u] (simplify $ map (\x -> (x,[])) $ dropFlag u fdeps_u) ++ simplifyMore fdeps_nu
        in liftFlags common_fas common_fdeps ++ simplifyMore (sd ++ ys)

      get_fa_hist :: [FaDep] -> [((Cabal.FlagName,Bool),Int)]
      get_fa_hist fdeps = reverse $! L.sortBy (compare `on` snd) $!
                                     M.toList $!
                                     go M.empty (concatMap fst fdeps)
            where go hist [] = hist
                  go hist (fd:fds) = go (M.insertWith (+) fd 1 hist) fds
      -- drop selected use flag from a list
      dropFlag :: (Cabal.FlagName,Bool) -> [FaDep] -> [FaDep]
      dropFlag f = map (first (filter (f /=)))
      hasFlag :: (Cabal.FlagName,Bool) -> FaDep -> Bool
      hasFlag u = elem u . fst

      liftFlags :: Cabal.FlagAssignment -> [Portage.Dependency] -> [Portage.Dependency]
      liftFlags fs e = let k = foldr (\(y,b) x -> Portage.mkUseDependency (b, Portage.Use . cfn_to_iuse . unFlagName $ y) . x)
                                      id fs
                       in [k $! Portage.DependAllOf e]

      cabal_to_emerge_dep :: Cabal.PackageDescription -> Merge.EDep
      cabal_to_emerge_dep cabal_pkg = Merge.resolveDependencies overlay cabal_pkg (Just compilerId) ghc_packages merged_cabal_pkg_name

  debug verbosity $ "buildDepends pkgDesc0 raw: " ++ Cabal.showPackageDescription pkgDesc0
  debug verbosity $ "buildDepends pkgDesc0: " ++ show (map display (Cabal.buildDepends pkgDesc0))
  debug verbosity $ "buildDepends pkgDesc:  " ++ show (map display (Cabal.buildDepends pkgDesc))

  notice verbosity $ "Accepted depends: " ++ show (map display accepted_deps)
  notice verbosity $ "Skipped  depends: " ++ show (map display skipped_deps)
  notice verbosity $ "Dead flags: " ++ show (map pp_fa irresolvable_flag_assignments)
  notice verbosity $ "Dropped  flags: " ++ show (map (unFlagName.fst) common_fa)
  -- mapM_ print tdeps

  forM_ ghc_packages $
      \(Cabal.PackageName name) -> info verbosity $ "Excluded packages (comes with ghc): " ++ name

  let pp_fn (Cabal.FlagName fn, True)  =     fn
      pp_fn (Cabal.FlagName fn, False) = '-':fn

      -- appends 's' to each line except the last one
      --  handy to build multiline shell expressions
      icalate _s []     = []
      icalate _s [x]    = [x]
      icalate  s (x:xs) = (x ++ s) : icalate s xs

      selected_flags :: ([Cabal.FlagName], Cabal.FlagAssignment) -> [String]
      selected_flags ([], []) = []
      selected_flags (active_fns, users_fas) = icalate " \\" $ "haskell-cabal_src_configure" : map snd (L.sortBy (compare `on` fst) flag_pairs)
          where flag_pairs :: [(String, String)]
                flag_pairs = active_pairs ++ users_pairs
                active_pairs = map (\fn -> (fn,                    "\t$(cabal_flag " ++ cfn_to_iuse fn ++ " " ++ fn ++ ")")) $ map unFlagName active_fns
                users_pairs  = map (\fa -> ((unFlagName . fst) fa, "\t--flag=" ++ pp_fn fa)) users_fas
      to_iuse x = let fn = unFlagName $ Cabal.flagName x
                      p  = if Cabal.flagDefault x then "+" else ""
                  in p ++ cfn_to_iuse fn

      ebuild =   (\e -> e { E.depend        = Merge.dep tdeps} )
               . (\e -> e { E.depend_extra  = Merge.dep_e tdeps } )
               . (\e -> e { E.rdepend       = Merge.rdep tdeps} )
               . (\e -> e { E.rdepend_extra = Merge.rdep_e tdeps } )
               . (\e -> e { E.src_configure = selected_flags (active_flags, user_specified_fas) } )
               . (\e -> e { E.iuse = E.iuse e ++ map to_iuse active_flag_descs })
               . ( case requested_cabal_flags of
                       Nothing  -> id
                       Just ucf -> (\e -> e { E.used_options  = E.used_options e ++ [("flags", ucf)] }))
               $ C2E.cabal2ebuild pkgDesc

  mergeEbuild verbosity existing_meta pkgdir ebuild
  when fetch $ do
    let cabal_pkgId = Cabal.packageId pkgDesc
        norm_pkgName = Cabal.packageName (Portage.normalizeCabalPackageId cabal_pkgId)
    fetchDigestAndCheck
      verbosity
      (overlayPath </> display cat </> display norm_pkgName)

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

mergeEbuild :: Verbosity -> EM.EMeta -> FilePath -> E.EBuild -> IO ()
mergeEbuild verbosity existing_meta pkgdir ebuild = do
  let edir = pkgdir
      elocal = E.name ebuild ++"-"++ E.version ebuild <.> "ebuild"
      epath = edir </> elocal
      emeta = "metadata.xml"
      mpath = edir </> emeta
      default_meta = BL.pack $ Portage.makeDefaultMetadata (E.long_desc ebuild)
  createDirectoryIfMissing True edir
  now <- TC.getCurrentTime

  let (existing_keywords, existing_license)  = (EM.keywords existing_meta, EM.license existing_meta)
      new_keywords = maybe (E.keywords ebuild) (map to_unstable) existing_keywords
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
  length s_ebuild' `seq` BL.writeFile epath (BL.pack s_ebuild')

  yet_meta <- doesFileExist mpath
  if not yet_meta -- TODO: add --force-meta-rewrite to opts
      then do notice verbosity $ "Writing " ++ emeta
              BL.writeFile mpath default_meta
      else do current_meta <- BL.readFile mpath
              when (current_meta /= default_meta) $
                  notice verbosity $ "Default and current " ++ emeta ++ " differ."

unFlagName :: Cabal.FlagName -> String
unFlagName (Cabal.FlagName fname) = fname

type FaDep = (Cabal.FlagAssignment, [Portage.Dependency])

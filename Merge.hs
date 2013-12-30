{-# LANGUAGE PatternGuards, BangPatterns #-}
module Merge
  ( merge
  , mergeGenericPackageDescription
  ) where

import Control.Arrow (first, second)
import Control.Monad.Error
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isSpace)
import Data.Function (on)
import Data.Maybe
import Data.Monoid
import qualified Data.List as L
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
                        , getDirectoryContents
                        , setCurrentDirectory
                        , createDirectoryIfMissing
                        , doesFileExist
                        )
import System.Cmd (system)
import System.FilePath ((</>))
import System.Exit
import Text.Printf

import qualified Cabal2Ebuild as C2E
import qualified Portage.EBuild as E
import Error as E

import Network.URI


import qualified Portage.PackageId as Portage
import qualified Portage.Version as Portage
import qualified Portage.Metadata as Portage
import qualified Portage.Overlay as Overlay
import qualified Portage.Resolve as Portage
import qualified Portage.Dependency as Portage

import qualified Portage.GHCCore as GHCCore

import qualified Merge.Dependencies as Merge

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

merge :: Verbosity -> Repo -> URI -> [String] -> FilePath -> IO ()
merge verbosity repo _serverURI args overlayPath = do
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
  mergeGenericPackageDescription verbosity overlayPath cat (packageDescription selectedPkg) True

mergeGenericPackageDescription :: Verbosity -> FilePath -> Portage.Category -> Cabal.GenericPackageDescription -> Bool -> IO ()
mergeGenericPackageDescription verbosity overlayPath cat pkgGenericDesc fetch = do
  overlay <- Overlay.loadLazy overlayPath
  let merged_cabal_pkg_name = Cabal.pkgName (Cabal.package (Cabal.packageDescription pkgGenericDesc))

  debug verbosity $ "searching for minimal suitable ghc version"
  (compilerId, ghc_packages, pkgDesc0, _flags, pix) <- case GHCCore.minimumGHCVersionToBuildPackage pkgGenericDesc of
              Just v  -> return v
              Nothing -> let cpn = display merged_cabal_pkg_name
                         in error $ unlines [ "mergeGenericPackageDescription: failed to find suitable GHC for " ++ cpn
                                            , "  You can try to merge the package manually:"
                                            , "  $ cabal unpack " ++ cpn
                                            , "  $ cd " ++ cpn ++ "*/"
                                            , "  # fix " ++ cpn ++ ".cabal"
                                            , "  $ hackport make-ebuild dev-haskell " ++ cpn ++ ".cabal"
                                            ]

      -- , Right (pkg_desc, picked_flags) <- return (packageBuildableWithGHCVersion gpd g)]
  let (accepted_deps, skipped_deps, dropped_deps) = partition_depends (Cabal.buildDepends pkgDesc0)
      pkgDesc = pkgDesc0 { Cabal.buildDepends = accepted_deps }
      cabal_flag_descs = Cabal.genPackageFlags pkgGenericDesc
      all_flags = map Cabal.flagName cabal_flag_descs
      make_fas  :: [Cabal.Flag] -> [Cabal.FlagAssignment]
      make_fas  [] = [[]]
      make_fas  (f:rest) = [ (Cabal.flagName f, is_enabled) : fas
                           | fas <- make_fas rest
                           , is_enabled <- [False, True]
                           ]
      all_possible_flag_assignments :: [Cabal.FlagAssignment]
      all_possible_flag_assignments = make_fas cabal_flag_descs

      pp_fa :: Cabal.FlagAssignment -> String
      pp_fa fa = L.intercalate ", " [ (if b then '+' else '-') : f
                                    | (Cabal.FlagName f, b) <- fa
                                    ]

      -- key idea is to generate all possible list of flags
      deps1 :: [(Cabal.FlagAssignment, Merge.EDep)]
      deps1  = [ (f `updateFa` fr, cabal_to_emerge_dep pkgDesc_filtered_bdeps)
               | f <- all_possible_flag_assignments
               , Right (pkgDesc1,fr) <- [GHCCore.finalizePackageDescription f
                                                                  (GHCCore.dependencySatisfiable pix)
                                                                  (GHCCore.platform)
                                                                  compilerId
                                                                  []
                                                                  pkgGenericDesc]
               -- drop circular deps and shipped deps
               , let (ad, _sd, _rd) = partition_depends (Cabal.buildDepends pkgDesc1)
               -- TODO: drop ghc libraries from tests depends as well
               -- (see deepseq in hackport-0.3.5 as an example)
               , let pkgDesc_filtered_bdeps = pkgDesc1 { Cabal.buildDepends = ad }
               ]
          where
            updateFa :: Cabal.FlagAssignment -> Cabal.FlagAssignment -> Cabal.FlagAssignment
            updateFa [] _ = []
            updateFa (x:xs) y = case lookup (fst x) y of
                                  -- TODO: when does this code get triggered?
                                  Nothing -> x:(updateFa xs y)
                                  Just y' -> (fst x,y'):(updateFa xs y)
      -- then remove all flags that can't be changed
      common_fas = L.foldl1' L.intersect $ map fst deps1
      common_flags = map fst common_fas
      active_flags = all_flags L.\\ common_flags
      active_flag_descs = filter (\x -> Cabal.flagName x `elem` active_flags) cabal_flag_descs
      -- flags that are failed to resolve
      deadFlags = filter (\x -> all (x/=) $ map fst deps1) all_possible_flag_assignments
      -- and finally prettify all deps:
      leave_only_dynamic_fas :: Cabal.FlagAssignment -> Cabal.FlagAssignment
      leave_only_dynamic_fas = filter (\fa -> all (fa /=) common_fas)

      optimize_fa_depends :: [([(Cabal.FlagName, Bool)], [Portage.Dependency])] -> [Portage.Dependency]
      optimize_fa_depends deps = Portage.sortDeps
                               . simplify $ map (\fdep -> (fdep,[])) $
                                   map (first leave_only_dynamic_fas) deps

      tdeps :: Merge.EDep
      tdeps = (L.foldl' (\x y -> x `mappend` (snd y)) mempty deps1){
            Merge.dep  = optimize_fa_depends $ map (second Merge.dep) deps1
          , Merge.rdep = optimize_fa_depends $ map (second Merge.rdep) deps1
          }

      pop_common_deps :: [FlagDepH] -> FlagDepH
      pop_common_deps xs =
           case pop_from_pairs xs of
                 []  -> error "impossible"
                 [x] -> x
                 r   -> pop_common_deps r
          where
            pop_from_pairs :: [FlagDepH] -> [FlagDepH]
            pop_from_pairs [] = []
            pop_from_pairs [y] = [y]
            pop_from_pairs (y1:y2:rest) = y1 `pop_from_pair` y2 : pop_from_pairs rest

            pop_from_pair :: FlagDepH -> FlagDepH -> FlagDepH
            pop_from_pair ((lfa, ld), lx) ((rfa, rd), rx) = ((fa, d), x)
                where fa = lfa `L.intersect` rfa
                      d  = Portage.simplify_deps $ ld `L.intersect` rd
                      x  = (lfa, filter (`notElem` rd) ld)
                         : (rfa, filter (`notElem` ld) rd)
                         : lx ++ rx

      simplify :: [FlagDepH] -> [Portage.Dependency]
      simplify xs =
        let -- extract common part of the depends
            -- filtering out empty groups
            ((fl,c), zs) = second (filter (not.null.snd)) $ pop_common_deps xs
            -- Regroup flags according to packages, i.e.
            -- if 2 groups of flagged deps containg same package, then
            -- extract common flags, but if common flags will be empty
            -- then remove repacked package from the result list.
            -- This is simplify packages but will not break if depend
            -- is required but non intersecting groups.
            mergeD :: (Cabal.FlagAssignment, Portage.Dependency)
                   -> [(Cabal.FlagAssignment, Portage.Dependency)]
                   -> [(Cabal.FlagAssignment, Portage.Dependency)]
            mergeD x [] = [x]
            mergeD x@(f1,d1) (t@(f2,d2):ts) =
              let is = f1 `L.intersect` f2
              in if d1 == d2
                      then if null is
                                then ts
                                else (is,d1):ts
                      else t:mergeD x ts
            sd :: [(Cabal.FlagAssignment, [Portage.Dependency])]
            sd = L.foldl' (\o (f,d) -> case lookup f o of
                                          Just ds -> (f,d:ds):filter ((f/=).fst) o
                                          Nothing -> (f,[d]):o
                       ) [] $ L.foldl' (\o n -> n `mergeD` o)
                                    []
                                    (concatMap (\(f,d) -> map ((,) f) d) zs)
            -- filter out splitted packages from common cgroup
            ys = filter (not.null.snd) $ map (second (filter (\d -> all (d/=)
                                                              (concatMap snd sd))
                                                     )) zs
            -- Now we need to find noniteracting use flags if they are then we
            -- don't need to simplify them more, and output as-is
            simplifyMore :: [(Cabal.FlagAssignment,[Portage.Dependency])] -> [Portage.Dependency]
            simplifyMore [] = []
            simplifyMore ws =
                let us = getMultiFlags ws
                    (u,_) = L.maximumBy (compare `on` snd) $ getMultiFlags ws
                    (xs', ls) = (hasFlag u) `L.partition` ws
                in if null us
                      then concatMap (\(a, b) -> liftFlags a b) ws
                      else liftFlags [u] (simplify $ map (\x -> (x,[])) $ dropFlag u xs')++simplifyMore ls
        in (liftFlags fl c) ++ simplifyMore (sd ++ ys)

      getMultiFlags :: [FlagDep] -> [((Cabal.FlagName,Bool),Int)]
      getMultiFlags ys = go [] (concatMap fst ys)
            where go a [] = a
                  go a (x:xs) = case lookup x a of
                                  Nothing -> go ((x,1):a) xs
                                  Just n  -> go ((x,n+1):filter ((x/=).fst) a) xs
      -- drop selected use flag from a list
      dropFlag :: (Cabal.FlagName,Bool) -> [FlagDep] -> [FlagDep]
      dropFlag f = map (first (filter (f /=)))
      hasFlag :: (Cabal.FlagName,Bool) -> FlagDep -> Bool
      hasFlag u = any ((u ==)) . fst

      liftFlags :: Cabal.FlagAssignment -> [Portage.Dependency] -> [Portage.Dependency]
      liftFlags fs e = let k = foldr (\(y,b) x -> Portage.DependIfUse (Portage.DUse (b, unFlagName y)) . x)
                                      (id::Portage.Dependency->Portage.Dependency) fs
                       in Portage.simplify_deps [k $! Portage.DependAllOf e]


      partition_depends :: [Cabal.Dependency] -> ([Cabal.Dependency], [Cabal.Dependency], [Cabal.Dependency])
      partition_depends =
          L.foldl' (\(ad, sd, rd) (Cabal.Dependency pn vr) ->
                  let dep = (Cabal.Dependency pn (Cabal.simplifyVersionRange vr))
                  in case () of
                        _ | pn `elem` ghc_packages      -> (    ad, dep:sd,     rd)
                        _ | pn == merged_cabal_pkg_name -> (    ad,     sd, dep:rd)
                        _                               -> (dep:ad,     sd,     rd)
                )
                ([],[],[])
      cabal_to_emerge_dep :: Cabal.PackageDescription -> Merge.EDep
      cabal_to_emerge_dep cabal_pkg = Merge.resolveDependencies overlay cabal_pkg (Just compilerId)

  debug verbosity $ "buildDepends pkgDesc0 raw: " ++ Cabal.showPackageDescription pkgDesc0
  debug verbosity $ "buildDepends pkgDesc0: " ++ show (map display (Cabal.buildDepends pkgDesc0))
  debug verbosity $ "buildDepends pkgDesc:  " ++ show (map display (Cabal.buildDepends pkgDesc))

  notice verbosity $ "Accepted depends: " ++ show (map display accepted_deps)
  notice verbosity $ "Skipped  depends: " ++ show (map display skipped_deps)
  notice verbosity $ "Dropped  depends: " ++ show (map display dropped_deps)
  notice verbosity $ "Dead flags: " ++ show (map pp_fa deadFlags)
  notice verbosity $ "Dropped  flags: " ++ show (map (unFlagName.fst) common_fas)
  -- mapM_ print tdeps

  forM_ ghc_packages $
      \(Cabal.PackageName name) -> info verbosity $ "Excluded packages (comes with ghc): " ++ name

  let -- p_flag (Cabal.FlagName fn, True)  =     fn
      -- p_flag (Cabal.FlagName fn, False) = '-':fn


      -- appends 's' to each line except the last one
      --  handy to build multiline shell expressions
      icalate _s []     = []
      icalate _s [x]    = [x]
      icalate  s (x:xs) = (x ++ s) : icalate s xs

      selected_flags :: [String] -> [String]
      selected_flags [] = []
      selected_flags fs = icalate " \\" $ "haskell-cabal_src_configure"
                                        : map (\p -> "\t$(cabal_flag "++ p ++" "++ p ++")") fs
      to_iuse x = let fn = unFlagName $ Cabal.flagName x
                      p  = if Cabal.flagDefault x then "+" else ""
                  in p++fn

      ebuild =   (\e -> e { E.depend        = Merge.dep tdeps} )
               . (\e -> e { E.depend_extra  = Merge.dep_e tdeps } )
               . (\e -> e { E.rdepend       = Merge.rdep tdeps} )
               . (\e -> e { E.rdepend_extra = Merge.rdep_e tdeps } )
               . (\e -> e { E.src_configure = selected_flags $ L.sort $ map unFlagName active_flags } )
               . (\e -> e { E.iuse = E.iuse e ++ map to_iuse active_flag_descs })
               $ C2E.cabal2ebuild pkgDesc

  mergeEbuild verbosity overlayPath (Portage.unCategory cat) ebuild
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

-- tries to extract value of variable in var="val" format
-- There should be exactly one variable assignment in ebuild
-- It's a bit artificial limitation, but it's common for 'if / else' blocks
extract_quoted_string :: FilePath -> String -> String -> Maybe String
extract_quoted_string ebuild_path s_ebuild var_name =
    case filter (L.isPrefixOf var_prefix . ltrim) $ lines s_ebuild of
        []        -> Nothing
        [kw_line] -> up_to_quote $ skip_prefix $ ltrim kw_line
        other     -> bail_out $ printf "strange '%s' assignmets:\n%s" var_name (unlines other)

    where ltrim :: String -> String
          ltrim = dropWhile isSpace
          var_prefix = var_name ++ "=\""
          skip_prefix = drop (length var_prefix)
          up_to_quote l = case break (== '"') l of
                              ("", _)  -> Nothing -- empty line
                              (_, "")  -> bail_out $ printf "failed to find closing quote for '%s'" l
                              (val, _) -> Just val
          bail_out :: String -> e
          bail_out msg = error $ printf "%s:extract_quoted_string %s" ebuild_path msg

extractKeywords :: FilePath -> String -> Maybe [String]
extractKeywords ebuild_path s_ebuild =
    words `fmap ` extract_quoted_string ebuild_path s_ebuild "KEYWORDS"

extractLicense :: FilePath -> String -> Maybe String
extractLicense ebuild_path s_ebuild =
    extract_quoted_string ebuild_path s_ebuild "LICENSE"

-- aggregated (best inferred) metadata for a new ebuild of package
data EMeta = EMeta { keywords :: Maybe [String]
                   , license  :: Maybe String
                   }

findExistingMeta :: FilePath -> IO EMeta
findExistingMeta edir =
    do ebuilds <- filter (L.isPrefixOf (reverse ".ebuild") . reverse) `fmap` getDirectoryContents edir
       -- TODO: version sort
       e_metas <- forM ebuilds $ \e ->
                      do let e_path = edir </> e
                         e_conts <- readFile e_path
                         return EMeta { keywords = extractKeywords e e_conts
                                      , license  = extractLicense  e e_conts
                                      }
       let get_latest candidates = last (Nothing : filter (/= Nothing) candidates)
           aggregated_meta = EMeta { keywords = get_latest $ map keywords e_metas
                                   , license  = get_latest $ map license e_metas
                                   }
       return $ aggregated_meta

-- "amd64" -> "~amd64"
to_unstable :: String -> String
to_unstable kw =
    case kw of
        '~':_ -> kw
        '-':_ -> kw
        _     -> '~':kw

mergeEbuild :: Verbosity -> FilePath -> String -> E.EBuild -> IO ()
mergeEbuild verbosity target cat ebuild = do
  let edir = target </> cat </> E.name ebuild
      elocal = E.name ebuild ++"-"++ E.version ebuild <.> "ebuild"
      epath = edir </> elocal
      emeta = "metadata.xml"
      mpath = edir </> emeta
      default_meta = BL.pack $ Portage.makeDefaultMetadata (E.long_desc ebuild)
  createDirectoryIfMissing True edir
  existing_meta <- findExistingMeta edir

  let (existing_keywords, existing_license)  = (keywords existing_meta, license existing_meta)
      new_keywords = maybe (E.keywords ebuild) (map to_unstable) existing_keywords
      new_license  = either (\err -> maybe (Left err)
                                           Right
                                           existing_license)
                            Right
                            (E.license ebuild)
      ebuild'      = ebuild { E.keywords = new_keywords
                            , E.license = new_license
                            }
      s_ebuild'    = display ebuild'

  notice verbosity $ "Current keywords: " ++ show existing_keywords ++ " -> " ++ show new_keywords
  notice verbosity $ "Current license:  " ++ show existing_license ++ " -> " ++ show new_license

  notice verbosity $ "Writing " ++ elocal
  (length s_ebuild') `seq` BL.writeFile epath (BL.pack s_ebuild')

  yet_meta <- doesFileExist mpath
  if (not yet_meta) -- TODO: add --force-meta-rewrite to opts
      then do notice verbosity $ "Writing " ++ emeta
              BL.writeFile mpath default_meta
      else do current_meta <- BL.readFile mpath
              when (current_meta /= default_meta) $
                  notice verbosity $ "Default and current " ++ emeta ++ " differ."

unFlagName :: Cabal.FlagName -> String
unFlagName (Cabal.FlagName fname) = fname

type FlagDep  = (Cabal.FlagAssignment,[Portage.Dependency])
type FlagDepH = (FlagDep,[FlagDep])

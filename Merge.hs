{-# LANGUAGE PatternGuards #-}
module Merge
  ( merge
  , mergeGenericPackageDescription
  ) where

import Control.Monad.Error
import Control.Exception
import qualified Data.ByteString.Lazy.Char8 as BL
import Data.Char (isSpace)
import Data.Maybe
import Data.Monoid
import Data.List as L
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

import qualified Cabal2Ebuild as C2E
import qualified Portage.EBuild as E
import Error as E

import Network.URI


import qualified Portage.PackageId as Portage
import qualified Portage.Version as Portage
import qualified Portage.Metadata as Portage
import qualified Portage.Overlay as Overlay
import qualified Portage.Resolve as Portage
import qualified Portage.Use     as Portage
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
resolveVersion avails Nothing = Just $ maximumBy (comparing packageInfoId) avails
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

  let Just (compilerId, ghc_packages, pkgDesc0, flags, pix) = GHCCore.minimumGHCVersionToBuildPackage pkgGenericDesc

      -- , Right (pkg_desc, picked_flags) <- return (packageBuildableWithGHCVersion gpd g)]
      (accepted_deps, skipped_deps, dropped_deps) = genSimple (Cabal.buildDepends pkgDesc0)
      pkgDesc = pkgDesc0 { Cabal.buildDepends = accepted_deps }
      aflags = map Cabal.flagName (Cabal.genPackageFlags pkgGenericDesc)
      lflags  :: [Cabal.Flag] -> [Cabal.FlagAssignment]
      lflags  [] = [[]]
      lflags  (x:xs) = let tp = lflags xs
                       in (map ((Cabal.flagName x,False) :) tp)
                          ++ (map ((Cabal.flagName x,True):) tp)
      deps1  = filter (not.null.fst)
               [ (sort $ map fst f', genDeps pkgDesc1)
               | f <- lflags (Cabal.genPackageFlags pkgGenericDesc)
               , Right (pkgDesc1,_) <- return (GHCCore.finalizePackageDescription f
                                                                  (GHCCore.dependencySatisfiable pix)
                                                                  (GHCCore.platform)
                                                                  compilerId
                                                                  []
                                                                  pkgGenericDesc)
               , f' <- return $ filter snd f
               ]
      cdeps1 = if null deps1 then mempty else L.foldl1 (Merge.intersection) $ map snd deps1
      fdeps1 = map (uncurry liftFlags1)
                $ filter (not . Merge.null . snd)
                $ map diffParts deps1
      diffParts (f, x) = (f, (foldl go x $ (filter (/= (sort f)) (map sort $ L.subsequences f))) `Merge.difference` cdeps1) -- ^ we need reverse to preserve ordering
         where go y l = case lookup l deps1 of
                          Nothing -> y
                          Just z  -> y `Merge.difference` z
      liftFlags1 :: [Cabal.FlagName] -> Merge.EDep -> Merge.EDep
      liftFlags1 fs e = let k =  foldr (\y x -> Portage.DependIfUse (Portage.mkQUse $ unFlagName y) . x)
                                      (id::Portage.Dependency->Portage.Dependency) fs
                        in e { Merge.dep = if  null (Merge.dep e)
                                                then []
                                                else Portage.simplify_deps [k $! Portage.AllOf (Merge.dep e)]
                             , Merge.rdep = if null (Merge.rdep e)
                                                then []
                                                else Portage.simplify_deps [k $! Portage.AllOf (Merge.rdep e)]}

      tdeps = L.foldl (<>) cdeps1 fdeps1

      genSimple =
          foldl (\(ad, sd, rd) (Cabal.Dependency pn vr) ->
                  let dep = (Cabal.Dependency pn (Cabal.simplifyVersionRange vr))
                  in case () of
                        _ | pn `elem` ghc_packages      -> (    ad, dep:sd,     rd)
                        _ | pn == merged_cabal_pkg_name -> (    ad,     sd, dep:rd)
                        _                               -> (dep:ad,     sd,     rd)
                )
                ([],[],[])
      genDeps x = Merge.resolveDependencies overlay x (Just compilerId)

  debug verbosity $ "buildDepends pkgDesc0: " ++ show (map display (Cabal.buildDepends pkgDesc0))
  debug verbosity $ "buildDepends pkgDesc:  " ++ show (map display (Cabal.buildDepends pkgDesc))

  notice verbosity $ "Accepted depends: " ++ show (map display accepted_deps)
  notice verbosity $ "Skipped  depends: " ++ show (map display skipped_deps)
  notice verbosity $ "Dropped  depends: " ++ show (map display dropped_deps)
  notice verbosity $ "Selected flags: " ++ show flags
  notice verbosity $ "Fixed depends: " ++ show (length deps1)

  forM_ ghc_packages $
      \(Cabal.PackageName name) -> info verbosity $ "Excluded packages (comes with ghc): " ++ name

  let -- p_flag (Cabal.FlagName fn, True)  =     fn
      -- p_flag (Cabal.FlagName fn, False) = '-':fn


      -- appends 's' to each line except the last one
      --  handy to build multiline shell expressions
      icalate _s []     = []
      icalate _s [x]    = [x]
      icalate  s (x:xs) = (x ++ s) : icalate s xs

      selected_flags [] = []
      selected_flags fs = icalate " \\" $ "haskell-cabal_src_configure"
                                        : map (\p -> "\t$(cabal_flag "++ p ++" "++ p ++")") fs

      ebuild =   (\e -> e { E.depend        = Merge.dep tdeps} )
               . (\e -> e { E.depend_extra  = Merge.dep_e tdeps } )
               . (\e -> e { E.rdepend       = Merge.rdep tdeps} )
               . (\e -> e { E.rdepend_extra = Merge.rdep_e tdeps } )
               . (\e -> e { E.src_configure = selected_flags (map unFlagName aflags) } )
               . (\e -> e { E.iuse = E.iuse e ++ map unFlagName aflags })
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

extractKeywords :: FilePath -> String -> Maybe [String]
extractKeywords ebuild_path s_ebuild =
    let ltrim :: String -> String
        ltrim = dropWhile isSpace
        lns    = lines s_ebuild
        -- TODO: nicer pattern match and errno
    in case (findIndices (isPrefixOf "KEYWORDS=\"" . ltrim) lns) of
           []      -> Nothing
           [kw_ln] -> let kw_line  = lns !! kw_ln
                          kw_str   = (fst . break (== '"') . tail . snd . break (== '"')) kw_line
                          keywords = words kw_str
                      in if null keywords
                             then Nothing
                             else Just keywords
           other   -> error $ ebuild_path ++ ": parse_ebuild: strange KEYWORDS lines: " ++ show other

findExistingKeywords :: FilePath -> IO (Maybe [String])
findExistingKeywords edir =
    do ebuilds <- filter (isPrefixOf (reverse ".ebuild") . reverse) `fmap` getDirectoryContents edir
       -- TODO: version sort
       e_kw_s <- forM ebuilds $ \e ->
                     do let e_path = edir </> e
                        e_conts <- readFile e_path
                        return (e, extractKeywords e_path e_conts)
       if null e_kw_s
           then return Nothing
           else return (snd $ last e_kw_s)

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
  existing_keywords <- findExistingKeywords edir

  let new_keywords = maybe (E.keywords ebuild) (map to_unstable) (existing_keywords)
      ebuild'      = ebuild { E.keywords = new_keywords }
      s_ebuild'    = display ebuild'

  notice verbosity $ "Current keywords: " ++ show existing_keywords ++ " -> " ++ show new_keywords

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
unFlagName f =
  let Cabal.FlagName y = f
  in y

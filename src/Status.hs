
-- TODO: Rearrange things so we don't have to disable this warning
{-# OPTIONS_GHC -Wno-incomplete-uni-patterns #-}

module Status
    ( status
    , runStatus
    ) where

import AnsiColor

import qualified Portage.Version as V (is_live)

import Portage.Overlay
import Portage.PackageId
import Portage.Resolve

import qualified Data.List as List

import qualified Data.ByteString.Char8 as BS

import Data.Char
import Data.Function (on)
import qualified Data.Map as Map
import Data.Map as Map (Map)

import qualified Data.Traversable as T
import Control.Monad

-- cabal
import qualified Distribution.Package as Cabal (pkgName)
import qualified Distribution.Simple.Utils as Cabal (die', equating)
import Distribution.Pretty (prettyShow)
import Distribution.Parsec (simpleParsec)

import qualified Distribution.Client.GlobalFlags as CabalInstall
import qualified Distribution.Client.IndexUtils as CabalInstall
import qualified Distribution.Client.Types as CabalInstall ( SourcePackageDb(..) )
import qualified Distribution.Solver.Types.PackageIndex as CabalInstall
import qualified Distribution.Solver.Types.SourcePackage as CabalInstall ( SourcePackage(..) )

import Overlays
import Status.Types
import Hackport.Env
import Hackport.Util



loadHackage
  :: CabalInstall.RepoContext
  -> Overlay
  -> Env env [[PackageId]]
loadHackage repoContext overlay = askGlobalEnv >>= \(GlobalEnv verbosity _ _) -> do
    CabalInstall.SourcePackageDb { CabalInstall.packageIndex = pindex } <-
      liftIO $ CabalInstall.getSourcePackages verbosity repoContext
    let get_cat cabal_pkg = case resolveCategories overlay (Cabal.pkgName cabal_pkg) of
                                []    -> Category "dev-haskell"
                                [cat] -> cat
                                _     -> {- ambig -} Category "dev-haskell"
        pkg_infos = map ( reverse . take 3 . reverse -- hackage usually has a ton of older versions
                        . map ((\p -> fromCabalPackageId (get_cat p) p)
                              . CabalInstall.srcpkgPackageId))
                        (CabalInstall.allPackagesByName pindex)
    return pkg_infos

status :: CabalInstall.RepoContext -> Env StatusEnv (Map PackageName [FileStatus ExistingEbuild])
status repoContext = do
    portdir <- getPortageDir
    overlaydir <- getOverlayPath
    overlay <- loadLazy overlaydir
    hackage <- loadHackage repoContext overlay
    portage <- filterByEmail ("haskell@gentoo.org" `elem`) <$> loadLazy portdir
    let (over, both, port) = portageDiff (overlayMap overlay) (overlayMap portage)

    both' <- T.forM both $ mapM $ \e -> do
            -- can't fail, we know the ebuild exists in both portagedirs
            -- also, one of them is already bound to 'e'
            let (Just e1) = lookupEbuildWith (overlayMap portage) (ebuildId e)
                (Just e2) = lookupEbuildWith (overlayMap overlay) (ebuildId e)
            eq <- equals (ebuildPath e1) (ebuildPath e2)
            return $ if eq
                        then Same e1
                        else Differs e1 e2

    let p_to_ee :: PackageId -> ExistingEbuild
        p_to_ee p = ExistingEbuild p cabal_p ebuild_path
            where Just cabal_p = toCabalPackageId p -- lame doubleconv
                  ebuild_path = packageIdToFilePath p
        mk_fake_ee :: [PackageId] -> (PackageName, [ExistingEbuild])
        mk_fake_ee ~pkgs@(p:_) = (packageId p, map p_to_ee pkgs)

        map_diff = Map.differenceWith (\le re -> Just $ foldr (List.deleteBy (Cabal.equating ebuildId)) le re)
        hack = (( -- We merge package names as we do case-insensitive match.
                  -- Hackage contains the following 2 package names:
                  --   ... Cabal-1.24.0.0 Cabal-1.24.1.0
                  --   cabal-0.0.0.0
                  -- We need to pick both lists of versions, not the first.
                  -- TODO: have a way to distict between them in the output.
                  Map.fromListWith (++) $
                      map mk_fake_ee hackage) `map_diff` overlayMap overlay) `map_diff` overlayMap portage

        meld = Map.unionsWith (\a b -> List.sort (a++b))
                [ Map.map (map PortageOnly) port
                , both'
                , Map.map (map OverlayOnly) over
                , Map.map (map HackageOnly) hack
                ]
    return meld

type EMap = Map PackageName [ExistingEbuild]

lookupEbuildWith :: EMap -> PackageId -> Maybe ExistingEbuild
lookupEbuildWith overlay pkgid = do
  ebuilds <- Map.lookup (packageId pkgid) overlay
  List.find (\e -> ebuildId e == pkgid) ebuilds

runStatus :: CabalInstall.RepoContext -> Env StatusEnv ()
runStatus repoContext =
  ask >>= \(GlobalEnv verbosity _ _, StatusEnv direction pkgs) -> do
  let pkgFilter = case direction of
                      OverlayToPortage   -> toPortageFilter
                      PortagePlusOverlay -> id
                      HackageToOverlay   -> fromHackageFilter
  pkgs' <- forM pkgs $ \p ->
            case simpleParsec p of
              Nothing -> liftIO $ Cabal.die' verbosity ("Could not parse package name: " ++ p ++ ". Format cat/pkg")
              Just pn -> return pn
  tree0 <- status repoContext
  let tree = pkgFilter tree0
  if (null pkgs')
    then statusPrinter tree
    else forM_ pkgs' $ \pkg -> statusPrinter (Map.filterWithKey (\k _ -> k == pkg) tree)

-- |Only return packages that seems interesting to sync to portage;
--
--   * Ebuild differs, or
--   * Newer version in overlay than in portage
toPortageFilter :: Map PackageName [FileStatus ExistingEbuild] -> Map PackageName [FileStatus ExistingEbuild]
toPortageFilter = Map.mapMaybe $ \ sts ->
    let filter_out_lives = filter (not . V.is_live . pkgVersion . ebuildId . fromStatus)
        inPortage = flip filter sts $ \st ->
                        case st of
                            OverlayOnly _ -> False
                            HackageOnly _ -> False
                            _ -> True
        latestPortageVersion = List.maximum $ map (pkgVersion . ebuildId . fromStatus) inPortage
        interestingPackages = flip filter sts $ \st ->
            case st of
                HackageOnly _ -> False
                Differs _ _ -> True
                _ | pkgVersion (ebuildId (fromStatus st)) > latestPortageVersion -> True
                  | otherwise -> False
    in if not (null inPortage) && not (null $ filter_out_lives interestingPackages)
        then Just sts
        else Nothing

-- |Only return packages that exist in overlay or portage but look outdated
fromHackageFilter :: Map PackageName [FileStatus ExistingEbuild] -> Map PackageName [FileStatus ExistingEbuild]
fromHackageFilter = Map.mapMaybe $ \ sts ->
    let inEbuilds = flip filter sts $ \st ->
                        case st of
                            HackageOnly _ -> False
                            _ -> True
        -- treat live as oldest version not avoid masking hackage releases
        mangle_live_versions v
            | V.is_live v = v {versionNumber=[-1]}
            | otherwise   = v
        latestVersion = List.maximumBy (compare `on` mangle_live_versions . pkgVersion . ebuildId . fromStatus) sts
    in case latestVersion of
            HackageOnly _ | not (null inEbuilds) -> Just sts
            _                                    -> Nothing

statusPrinter :: MonadIO m => Map PackageName [FileStatus ExistingEbuild] -> m ()
statusPrinter packages = liftIO $ do
    putStrLn $ toColor (Same "Green") ++ ": package in portage and overlay are the same"
    putStrLn $ toColor (Differs "Yellow" "") ++ ": package in portage and overlay differs"
    putStrLn $ toColor (OverlayOnly "Red") ++ ": package only exist in the overlay"
    putStrLn $ toColor (PortageOnly "Magenta") ++ ": package only exist in the portage tree"
    putStrLn $ toColor (HackageOnly "Cyan") ++ ": package only exist on hackage"
    forM_ (zip [(1 :: Int) ..] $ Map.toAscList packages) $ \(ix, (pkg, ebuilds)) -> do
        let (PackageName c p) = pkg
        putStr (bold (show ix))
        putStr " "
        putStr $ prettyShow c ++ '/' : bold (prettyShow p)
        putStr " "
        forM_ ebuilds $ \e -> do
            putStr $ toColor (fmap (prettyShow . pkgVersion . ebuildId) e)
            putChar ' '
        putStrLn ""

toColor :: FileStatus String -> String
toColor st = inColor c False Default (fromStatus st)
    where
    c = case st of
        (Same _) -> Green
        (Differs _ _) -> Yellow
        (OverlayOnly _) -> Red
        (PortageOnly _) -> Magenta
        (HackageOnly _) -> Cyan

portageDiff :: EMap -> EMap -> (EMap, EMap, EMap)
portageDiff p1 p2 = (in1, ins, in2)
    where ins = Map.filter (not . null) $ Map.intersectionWith (List.intersectBy $ Cabal.equating ebuildId) p1 p2
          in1 = difference p1 p2
          in2 = difference p2 p1
          difference x y = Map.filter (not . null) $
                       Map.differenceWith (\xs ys ->
                        let lst = foldr (List.deleteBy (Cabal.equating ebuildId)) xs ys in
                        if null lst
                            then Nothing
                            else Just lst
                            ) x y

-- | Compares two ebuilds, returns True if they are equal.
--   Disregards comments.
equals :: MonadIO m => FilePath -> FilePath -> m Bool
equals fp1 fp2 = liftIO $ do
    -- don't leave halfopenfiles
    f1 <- BS.readFile fp1
    f2 <- BS.readFile fp2
    return (equal' f1 f2)

equal' :: BS.ByteString -> BS.ByteString -> Bool
equal' = Cabal.equating essence
    where
    essence = filter (not . isEmpty)
            . filter (not . isComment)
            . filter (not . isHOMEPAGE)
            . BS.lines
    isComment = BS.isPrefixOf (BS.pack "#") . BS.dropWhile isSpace
    -- HOMEPAGE= frequently gets updated for http:// / https://.
    -- It's to much noise usually and should really be fixed
    -- in upstream Cabal definition.
    isHOMEPAGE = BS.isPrefixOf (BS.pack "HOMEPAGE=") . BS.dropWhile isSpace
    isEmpty = BS.null . BS.dropWhile isSpace

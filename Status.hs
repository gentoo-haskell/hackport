module Status
    ( FileStatus(..)
    , StatusDirection(..)
    , fromStatus
    , status
    , runStatus
    ) where

import AnsiColor

import qualified Data.Version as V()

import Portage.Overlay
import Portage.PackageId
import Portage.Resolve

import Control.Monad.State

import qualified Data.List as List

import qualified Data.ByteString.Lazy.Char8 as L

import Data.Char
import Data.Function (on)
import qualified Data.Map as Map
import Data.Map as Map (Map)

import qualified Data.Traversable as T
import Control.Applicative

-- cabal
import Distribution.Client.Types ( Repo, SourcePackageDb(..), SourcePackage(..) )
import Distribution.Verbosity
import Distribution.Package (pkgName)
import Distribution.Simple.Utils (comparing, die, equating)
import Distribution.Text ( display, simpleParse )

import qualified Distribution.Client.PackageIndex as CabalInstall
import qualified Distribution.Client.IndexUtils as CabalInstall

import Hackage (defaultRepo)

data StatusDirection
    = PortagePlusOverlay
    | OverlayToPortage
    | HackageToOverlay
    deriving Eq

data FileStatus a
        = Same a
        | Differs a a
        | OverlayOnly a
        | PortageOnly a
        | HackageOnly a
        deriving (Show,Eq)

instance Ord a => Ord (FileStatus a) where
    compare = comparing fromStatus

instance Functor FileStatus where
    fmap f st =
        case st of
            Same a -> Same (f a)
            Differs a b -> Differs (f a) (f b)
            OverlayOnly a -> OverlayOnly (f a)
            PortageOnly a -> PortageOnly (f a)
            HackageOnly a -> HackageOnly (f a)

fromStatus :: FileStatus a -> a
fromStatus fs =
    case fs of
        Same a -> a
        Differs a _ -> a -- second status is lost
        OverlayOnly a -> a
        PortageOnly a -> a
        HackageOnly a -> a



loadHackage :: Verbosity -> Distribution.Client.Types.Repo -> Overlay -> IO [[PackageId]]
loadHackage verbosity repo overlay = do
    SourcePackageDb { packageIndex = pindex } <- CabalInstall.getSourcePackages verbosity [repo]
    let get_cat cabal_pkg = case resolveCategories overlay (pkgName cabal_pkg) of
                                [cat] -> cat
                                _     -> {- ambig -} Category "dev-haskell"
        pkg_infos = map ( reverse . take 3 . reverse -- hackage usually has a ton of older versions
                        . map ((\p -> fromCabalPackageId (get_cat p) p)
                              . packageInfoId))
                        (CabalInstall.allPackagesByName pindex)
    return pkg_infos

status :: Verbosity -> FilePath -> FilePath -> IO (Map PackageName [FileStatus ExistingEbuild])
status verbosity portdir overlaydir = do
    let repo = defaultRepo overlaydir
    overlay <- loadLazy overlaydir
    hackage <- loadHackage verbosity repo overlay
    portage <- filterByHerd ("haskell" `elem`) <$> loadLazy portdir
    let (over, both, port) = portageDiff (overlayMap overlay) (overlayMap portage)

    both' <- T.forM both $ mapM $ \e -> liftIO $ do
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

        map_diff = Map.differenceWith (\le re -> Just $ foldr (List.deleteBy (equating ebuildId)) le re)
        hack = ((Map.fromList $ map mk_fake_ee hackage) `map_diff` overlayMap overlay) `map_diff` overlayMap portage

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

runStatus :: Verbosity -> FilePath -> FilePath -> StatusDirection -> [String] -> IO ()
runStatus verbosity portdir overlaydir direction pkgs = do
  let pkgFilter = case direction of
                      OverlayToPortage   -> toPortageFilter
                      PortagePlusOverlay -> id
                      HackageToOverlay   -> fromHackageFilter
  pkgs' <- forM pkgs $ \p ->
            case simpleParse p of
              Nothing -> die ("Could not parse package name: " ++ p ++ ". Format cat/pkg")
              Just pn -> return pn
  tree0 <- status verbosity portdir overlaydir
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
    let inPortage = flip filter sts $ \st ->
                        case st of
                            OverlayOnly _ -> False
                            HackageOnly _ -> False
                            _ -> True
        latestPortageVersion = List.maximum $ map (pkgVersion . ebuildId . fromStatus) inPortage
        interestingPackages = flip filter sts $ \st ->
            case st of
                Differs _ _ -> True
                _ | pkgVersion (ebuildId (fromStatus st)) > latestPortageVersion -> True
                  | otherwise -> False
    in if not (null inPortage) && not (null interestingPackages)
        then Just sts
        else Nothing

-- |Only return packages that exist in overlay or portage but look outdated
fromHackageFilter :: Map PackageName [FileStatus ExistingEbuild] -> Map PackageName [FileStatus ExistingEbuild]
fromHackageFilter = Map.mapMaybe $ \ sts ->
    let inEbuilds = flip filter sts $ \st ->
                        case st of
                            HackageOnly _ -> False
                            _ -> True
        latestVersion = List.maximumBy (compare `on` pkgVersion . ebuildId . fromStatus) sts
    in case latestVersion of
            HackageOnly _ | not (null inEbuilds) -> Just sts
            _                                    -> Nothing

statusPrinter :: Map PackageName [FileStatus ExistingEbuild] -> IO ()
statusPrinter packages = do
    putStrLn $ toColor (Same "Green") ++ ": package in portage and overlay are the same"
    putStrLn $ toColor (Differs "Yellow" "") ++ ": package in portage and overlay differs"
    putStrLn $ toColor (OverlayOnly "Red") ++ ": package only exist in the overlay"
    putStrLn $ toColor (PortageOnly "Magenta") ++ ": package only exist in the portage tree"
    putStrLn $ toColor (HackageOnly "Cyan") ++ ": package only exist on hackage"
    forM_ (Map.toAscList packages) $ \(pkg, ebuilds) -> do
        let (PackageName c p) = pkg
        putStr $ display c ++ '/' : bold (display p)
        putStr " "
        forM_ ebuilds $ \e -> do
            putStr $ toColor (fmap (display . pkgVersion . ebuildId) e)
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
    where ins = Map.filter (not . null) $ Map.intersectionWith (List.intersectBy $ equating ebuildId) p1 p2
          in1 = difference p1 p2
          in2 = difference p2 p1
          difference x y = Map.filter (not . null) $
                       Map.differenceWith (\xs ys ->
                        let lst = foldr (List.deleteBy (equating ebuildId)) xs ys in
                        if null lst
                            then Nothing
                            else Just lst
                            ) x y

-- | Compares two ebuilds, returns True if they are equal.
--   Disregards comments.
equals :: FilePath -> FilePath -> IO Bool
equals fp1 fp2 = do
    f1 <- L.readFile fp1
    f2 <- L.readFile fp2
    return (equal' f1 f2)

equal' :: L.ByteString -> L.ByteString -> Bool
equal' = equating essence
    where
    essence = filter (not . isEmpty) . filter (not . isComment) . L.lines
    isComment = L.isPrefixOf (L.pack "#") . L.dropWhile isSpace
    isEmpty = L.null . L.dropWhile isSpace

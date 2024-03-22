{-# LANGUAGE TupleSections #-}

module Hackport.Completion
    ( categoryCompleter
    , cabalPackageCompleter
    , trieFile
    ) where

import Control.Applicative
import Control.Monad.Trans.Maybe
import Data.Binary
import qualified Data.List as L
import Data.Maybe (catMaybes)
import qualified Data.Set as Set
import Data.Set (Set)
import qualified Options.Applicative as Opt
import System.Exit (ExitCode(..))
import System.FilePath
import System.IO.Error (tryIOError)
import System.Process hiding (env)
import Text.PrettyPrint (render)

import qualified Data.Trie as Trie
import Data.Trie (Trie)
import Hackport.Dirs (hackportDir)
import Hackport.Env
import Hackport.Util (withHackportContext)
import Util

import Distribution.Client.GlobalFlags (RepoContext(..))
import Distribution.Client.IndexUtils (getSourcePackages)
import Distribution.Client.Types (packageIndex)
import Distribution.Compat.Parsing (choice)
import Distribution.Pretty (Pretty, pretty)
import Distribution.Solver.Types.PackageIndex (allPackages)
import Distribution.Solver.Types.SourcePackage (SourcePackage(..))
import qualified Distribution.Verbosity as V

-- | Read the @profiles/category@ file from:
--
--   * @gentoo@  (determined by portageq)
--
--   The following might be read from in the future but it probably isn't
--   worth it for the extra time taken -- there most likely won't be any
--   extra categories that don't exist in @::gentoo@:
--
--   * @haskell@ (determined by portageq)
--   * The repository reported by 'getOverlayPath'
--
--   This is used exclusively for shell completion for the @make-ebuild@
--   subcommand.
categoryCompleter :: Opt.Completer
categoryCompleter = Opt.mkCompleter $ \s -> do
    let env = GlobalEnv V.silent Nothing Nothing
    gentoo <- runEnv gentooOverlay () env

    let repos = catMaybes [gentoo]

    sets <- traverse repoCategories repos
    pure $ Set.toAscList $ Set.filter (s `L.isPrefixOf`) $ Set.unions sets

  where
    gentooOverlay :: Env () (Maybe FilePath)
    gentooOverlay = getRepoPath "gentoo"

    getRepoPath :: String -> Env () (Maybe FilePath)
    getRepoPath repo =
        runOrDie
            "/usr/bin/portageq"
            ["get_repo_path", "/", repo]
            $ \out -> case out of
                [p] -> Just p
                _   -> Nothing

    repoCategories :: FilePath -> IO (Set String)
    repoCategories repoPath = do
        let catsFile = repoPath </> "profiles" </> "categories"
        foldMap (Set.fromList . filter (not . null) . lines)
            -- Returns a Nothing (wrapped in IO) if `readFile` fails
            <$> optional (readFile catsFile)

-- | Read a list of package names/verisons on Hackage. This requires the user
--   to have run @hackport update@ at least once.
--
--   Internally, this creates a 'Trie' which is cached in
--   @~/.hackport/packages.cache@. The cache is removed when @hackport update@
--   is called, which causes the cache file to be regenerated.
--
--   This is used exclusively for shell completion for the @merge@
--   subcommand.
cabalPackageCompleter :: Opt.Completer
cabalPackageCompleter = Opt.mkCompleter $ \s -> do
    Just trie <- runMaybeT $ choice
        [ -- Check to see if there is an existing cache.
          readTrie
          -- Otherwise, create a new trie from the Hackage database
        , lift $ runEnv createTrie () $ GlobalEnv V.silent Nothing Nothing
        ]

    let endings = map fst $ maybe [] Trie.toList
            $ Trie.lookup trie s

    pure $ map (s ++) endings

  where

    -- A (Trie Char ()) is useful for storing/searching a list of strings
    createTrie :: Env () (Trie Char ())
    createTrie = withHackportContext $ \_ repoContext -> do
        pkgs <- allPV repoContext
        let trie = foldMap (\s -> Trie.singleton s ()) pkgs
        writeTrie trie
        pure trie

    readTrie :: MonadIO m
        => MaybeT m (Trie Char ())
    readTrie = MaybeT $ liftIO $ optional
        $ trieFile >>= decodeFile

    writeTrie :: MonadIO m => Trie Char () -> m ()
    writeTrie t = do
        f <- trieFile
        liftIO $ encodeFile f t

    allPV :: MonadIO m => RepoContext -> m [String]
    allPV repoContext = do
        index <- liftIO $ packageIndex <$> getSourcePackages V.silent repoContext
        let allP = allPackages index
        pure $ (\(SourcePackage i _ _ _) -> showPretty i) <$> allP

trieFile :: MonadIO m => m FilePath
trieFile = do
    d <- hackportDir
    pure $ d </> "packages" <.> "cache"

runOrDie :: FilePath -> [String] -> ([String] -> Maybe a) -> Env env (Maybe a)
runOrDie p ss f = do
    r <- liftIO
        $ tryIOError
        $ readProcessWithExitCode p ss ""
    case r of
        Left  e            -> die (show e)
        Right (ec, out, err) -> do
            case ec of
                ExitSuccess ->
                    pure $ f $ filter (not . null) $ lines $ out
                _ -> die $ unwords [out, err]

showPretty :: Pretty a => a -> String
showPretty = render . pretty

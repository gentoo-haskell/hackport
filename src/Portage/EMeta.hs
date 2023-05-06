{-|
Module      : Portage.EMeta
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Functions to propagate existing ebuild information
(such as its licence, description, switched flags etc.) to
a new ebuild.
-}
module Portage.EMeta
  ( EMeta(..)
  , findExistingMeta
  ) where

import Control.Monad
import Control.Monad.IO.Class
import Data.Char (isSpace)
import qualified Data.List as L

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Text.Printf

-- | Extract a value of variable in \'var=\"val\"\' format.
-- There should be exactly one variable assignment in the ebuild.
-- It's a bit of an artificial limitation, but it's common for \'if / else\' blocks.
extract_quoted_string :: FilePath -> String -> String -> Maybe String
extract_quoted_string ebuild_path s_ebuild var_name =
    case filter (L.isPrefixOf var_prefix . ltrim) $ lines s_ebuild of
        []        -> Nothing
        [kw_line] -> up_to_quote $ skip_prefix $ ltrim kw_line
        other     -> bail_out $ printf "strange '%s' assignments:\n%s" var_name (unlines other)

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

-- | Extract a value of variable in \'#hackport: var: val\' format.
-- There should be exactly one variable assignment in the ebuild.
extract_hackport_var :: FilePath -> String -> String -> Maybe String
extract_hackport_var ebuild_path s_ebuild var_name =
    case filter (L.isPrefixOf var_prefix) $ lines s_ebuild of
        []         -> Nothing
        [var_line] -> Just $ skip_prefix var_line
        other      -> bail_out $ printf "strange '%s' assignmets:\n%s" var_name (unlines other)

    where var_prefix = "#hackport: " ++ var_name ++ ": "
          skip_prefix = drop (length var_prefix)
          bail_out :: String -> e
          bail_out msg = error $ printf "%s:extract_hackport_var %s" ebuild_path msg

-- | Extract the existing keywords from an ebuild.
extractKeywords :: FilePath -> String -> Maybe [String]
extractKeywords ebuild_path s_ebuild =
    words `fmap ` extract_quoted_string ebuild_path s_ebuild "KEYWORDS"

-- | Extract the existing license from an ebuild.
extractLicense :: FilePath -> String -> Maybe String
extractLicense ebuild_path s_ebuild =
    extract_quoted_string ebuild_path s_ebuild "LICENSE"

-- | Extract the existing Cabal flags from an ebuild.
extractCabalFlags :: FilePath -> String -> Maybe String
extractCabalFlags ebuild_path s_ebuild =
    extract_hackport_var ebuild_path s_ebuild "flags"

-- | Extract the existing description from an ebuild.
extractDescription :: FilePath -> String -> Maybe String
extractDescription ebuild_path s_ebuild =
    extract_quoted_string ebuild_path s_ebuild "DESCRIPTION"

-- | Type representing the aggregated (best inferred) metadata for a
-- new ebuild of a package.
data EMeta = EMeta { keywords :: Maybe [String]
                   , license  :: Maybe String
                   , cabal_flags :: Maybe String
                   , description :: Maybe String
                   }

-- | Find the existing package metadata from the last available ebuild.
findExistingMeta :: MonadIO m => FilePath -> m EMeta
findExistingMeta pkgdir = liftIO $ do
    ebuilds <- filter (L.isSuffixOf ".ebuild") <$> do
        b <- doesDirectoryExist pkgdir
        if b
            then getDirectoryContents pkgdir
            else pure []
    -- TODO: version sort
    eMetas <- forM ebuilds $ \e -> do
        let ePath = pkgdir </> e
        eConts <- readFile ePath
        pure EMeta
            { keywords = extractKeywords e eConts
            , license = extractLicense e eConts
            , cabal_flags = extractCabalFlags e eConts
            , description = extractDescription e eConts
            }
    let get_latest candidates = last (Nothing : filter (/= Nothing) candidates)
        aggregated_meta = EMeta
            { keywords = get_latest $ map keywords eMetas
            , license = get_latest $ map license eMetas
            , cabal_flags = get_latest $ map cabal_flags eMetas
            , description = get_latest $ map description eMetas
            }
    pure aggregated_meta

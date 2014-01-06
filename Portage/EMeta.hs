module Portage.EMeta
  ( EMeta(..)
  , findExistingMeta
  ) where

import Control.Monad.Error
import Data.Char (isSpace)
import qualified Data.List as L

import System.Directory (doesDirectoryExist, getDirectoryContents)
import System.FilePath ((</>))
import Text.Printf

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
findExistingMeta pkgdir =
    do ebuilds <- filter (L.isSuffixOf ".ebuild") `fmap` do b <- doesDirectoryExist pkgdir
                                                            if b then getDirectoryContents pkgdir
                                                                 else return []
       -- TODO: version sort
       e_metas <- forM ebuilds $ \e ->
                      do let e_path = pkgdir </> e
                         e_conts <- readFile e_path
                         return EMeta { keywords = extractKeywords e e_conts
                                      , license  = extractLicense  e e_conts
                                      }
       let get_latest candidates = last (Nothing : filter (/= Nothing) candidates)
           aggregated_meta = EMeta { keywords = get_latest $ map keywords e_metas
                                   , license  = get_latest $ map license e_metas
                                   }
       return aggregated_meta

{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE ExistentialQuantification #-}
{-# LANGUAGE FlexibleContexts #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE RecordWildCards #-}
{-# LANGUAGE TupleSections #-}

module Portage.Metadata.RemoteId
    (
      -- * Remote id
      RemoteId (..)
      -- * Pretty printing
    , prettyPrintRemoteIds
    , prettyPrintRemoteId
      -- * Parsing
    , URIParser (..)
    , matchURIs
    , matchURI
    , runUriParser
    , definedParsers
      -- ** Individual parsers
    , hackageParser
    , bitbucketParser
    , codebergParser
    , cpanParser
    , cranParser
    , ctanParser
    , freedesktopParser
    , gentooParser
    , githubParser
    , gitlabParser
    , gnomeParser
    , kdeParser
    , launchpadParser
    , osdnParser
    , peclParser
    , pypiParser
    , rubygemsParser
    , savannahParser
    , savannahNonGnuParser
    , sourceforgeParser
    , sourcehutParser
    , vimParser
      -- ** Utility
      -- *** URI scheme
    , httpScheme
      -- *** URI domain
    , Domain
    , domainOrWWW
    , subdomain
      -- *** URI path
    , Path
    , stripPrefix
    , stripPrefixP
    , gitPath
      -- *** Misc
    , ignore
    , allChars
    ) where

import Control.Monad
import Data.Foldable (asum)
import qualified Data.List as L
import Data.Maybe (catMaybes, mapMaybe)
import qualified Data.Set as S
import Network.URI (URI(..), URIAuth(..), parseURI)
import System.FilePath.Posix
import Text.Parsec
import Text.Parsec.String

-- | Many of these are unlikely to source any Haskell code, but they are added
-- for completeness. The full list can be seen at:
-- <https://github.com/pkgcore/pkgcheck/blob/master/src/pkgcheck/checks/network.py>
data RemoteId
    = RemoteIdHackage String       -- ^ Hackage package
    | RemoteIdBitbucket String String -- ^ Bitbucket project
    | RemoteIdCodeberg String String -- ^ Codeberg project
    | RemoteIdCPAN String          -- ^ Perl package
    | RemoteIdCRAN String          -- ^ CRAN package
    | RemoteIdCTAN String          -- ^ CTAN package
    | RemoteIdFreedesktop String String -- ^ Freedesktop GitLab project
    | RemoteIdGentoo String        -- ^ Gentoo project
    | RemoteIdGithub String String -- ^ Github user and repo
    | RemoteIdGitlab String String -- ^ Gitlab user and repo
    | RemoteIdGnome String String  -- ^ Gnome GitLab project
    | RemoteIdKDE String String    -- ^ KDE Invent project
    | RemoteIdLaunchpad String     -- ^ Launchpad project
    | RemoteIdOSDN String          -- ^ OSDN project
    | RemoteIdPECL String          -- ^ PECL package
    | RemoteIdPyPI String          -- ^ PyPI project
    | RemoteIdRubygems String      -- ^ Rubygems gem
    | RemoteIdSavannah String      -- ^ GNU Savannah project
    | RemoteIdSavannahNonGNU String -- ^ NonGNU Savannah project
    | RemoteIdSourceforge String   -- ^ Sourceforge project
    | RemoteIdSourcehut String String -- ^ sourcehut project
    | RemoteIdVim String           -- ^ Vim script
    deriving (Show, Eq, Ord)

-- | A set of parsers to use on a 'URI'. Each parser can produce an arbitrary
--   type. These intermediate types are coalesced in 'mkRemoteId' and
--   are hidden from the top-level using the @ExistentialQuantification@
--   language extension.
data URIParser = forall scheme user regname port path query fragment. URIParser
    { schemeParser :: Parser scheme     -- | ^ scheme
    , userParser :: Parser user         -- | ^ user
    , regnameParser :: Parser regname   -- | ^ domain
    , portParser :: Parser port         -- | ^ port
    , pathParser :: Parser path         -- | ^ path
    , queryParser :: Parser query       -- | ^ query
    , fragmentParser :: Parser fragment -- | ^ fragment
      -- | coalescing function
    , mkRemoteId :: scheme -> user -> regname -> port -> path -> query -> fragment -> RemoteId
    }

type Domain = String
type Path = String

-- | Pretty print a 'S.Set' of 'RemoteId's as XML. Wraps the block in @"<upstream>"@.
prettyPrintRemoteIds :: S.Set RemoteId -> [String]
prettyPrintRemoteIds set
    | S.null set = []
    | otherwise =
        ["\t<upstream>"]
        ++ (prettyPrintRemoteId <$> S.toAscList set)
        ++ ["\t</upstream>"]

-- | Pretty print a single 'RemoteId'.
prettyPrintRemoteId :: RemoteId -> String
prettyPrintRemoteId = \case
    RemoteIdHackage p     -> pp "hackage"     p
    RemoteIdBitbucket u r -> pp "bitbucket"   $ u ++ "/" ++ r
    RemoteIdCodeberg u r  -> pp "codeberg"    $ u ++ "/" ++ r
    RemoteIdCPAN p        -> pp "cpan"        p
    RemoteIdCRAN p        -> pp "cran"        p
    RemoteIdCTAN p        -> pp "ctan"        p
    RemoteIdFreedesktop u r -> pp "freedesktop-gitlab" $ u ++ "/" ++ r
    RemoteIdGentoo p      -> pp "gentoo"      p
    RemoteIdGithub u r    -> pp "github"      $ u ++ "/" ++ r
    RemoteIdGitlab u r    -> pp "gitlab"      $ u ++ "/" ++ r
    RemoteIdGnome u r     -> pp "gnome-gitlab" $ u ++ "/" ++ r
    RemoteIdLaunchpad p   -> pp "launchpad"   p
    RemoteIdKDE u r       -> pp "kde-invent"  $ u ++ "/" ++ r
    RemoteIdOSDN p        -> pp "osdn"        p
    RemoteIdPECL p        -> pp "pecl"        p
    RemoteIdPyPI p        -> pp "pypi"        p
    RemoteIdRubygems g    -> pp "rubygems"    g
    RemoteIdSavannah p    -> pp "savannah"    p
    RemoteIdSavannahNonGNU p -> pp "savannah-nongnu" p
    RemoteIdSourceforge p -> pp "sourceforge" p
    RemoteIdSourcehut u r -> pp "sourcehut"   $ u ++ "/" ++ r
    RemoteIdVim s         -> pp "vim"         s
  where
    pp t v = "\t\t<remote-id type=\"" ++ t ++ "\">" ++ v ++ "</remote-id>"

-- | Run 'matchURI' on all given strings, collecting the result in a 'S.Set'.
matchURIs :: [String] -> S.Set RemoteId
matchURIs = S.fromList . mapMaybe matchURI

-- | Try to parse the given string using any of the defined URI parsers
matchURI :: String -> Maybe RemoteId
matchURI str = asum $ map runUriP definedParsers
  where
    runUriP :: URIParser -> Maybe RemoteId
    runUriP p = eitherToMaybe $ runUriParser p str

    -- @Maybe@ is an Alternative, whereas @Either e@ is not. This is needed
    -- to make 'asum' work.
    eitherToMaybe :: Either e a -> Maybe a
    eitherToMaybe (Left  _) = Nothing
    eitherToMaybe (Right x) = Just x

-- | All parsers defined in this module
definedParsers :: [URIParser]
definedParsers =
    [ hackageParser
    , bitbucketParser
    , codebergParser
    , cpanParser
    , cranParser
    , ctanParser
    , freedesktopParser
    , gentooParser
    , githubParser
    , gitlabParser
    , gnomeParser
    , kdeParser
    , launchpadParser
    , osdnParser
    , peclParser
    , pypiParser
    , rubygemsParser
    , savannahParser
    , savannahNonGnuParser
    , sourceforgeParser
    , sourcehutParser
    , vimParser
    ]

-- | @'hackage': 'https://hackage.haskell.org/package/{project}'@
hackageParser :: URIParser
hackageParser = URIParser
    httpScheme
    ignore
    (string "hackage.haskell.org")
    ignore
    (do
        (p:_) <- stripPrefixP "/package"
        pure p
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdHackage p)

-- | @"bitbucket": "https://bitbucket.org/{project}"@
bitbucketParser :: URIParser
bitbucketParser = URIParser
    (choice [httpScheme, string "git:"])
    ignore
    (domainOrWWW "bitbucket.org")
    ignore
    (do
        (u:r:_) <- stripPrefixP "/"
        (u,) <$> gitPath r
    )
    ignore
    ignore
    (\_ _ _ _ (u,r) _ _ -> RemoteIdBitbucket u r)

-- | @"codeberg": "https://codeberg.org/{project}"@
codebergParser :: URIParser
codebergParser = URIParser
    (choice [httpScheme, string "git:"])
    ignore
    (domainOrWWW "codeberg.org")
    ignore
    (do
        (u:r:_) <- stripPrefixP "/"
        (u,) <$> gitPath r
    )
    ignore
    ignore
    (\_ _ _ _ (u,r) _ _ -> RemoteIdCodeberg u r)

-- | @"cpan": "https://metacpan.org/dist/{project}"@
cpanParser :: URIParser
cpanParser = URIParser
    httpScheme
    ignore
    (string "metacpan.org")
    ignore
    (do
        (p:_) <- stripPrefixP "/dist"
        pure p
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdCPAN p)

-- | @'cran': 'https://cran.r-project.org/web/packages/{project}/'@
cranParser :: URIParser
cranParser = URIParser
    httpScheme
    ignore
    (string "cran.r-project.org")
    ignore
    (do
        (p:_) <- stripPrefixP "/web/packages"
        pure p
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdCRAN p)

-- | @'ctan': 'https://ctan.org/pkg/{project}'@
ctanParser :: URIParser
ctanParser = URIParser
    httpScheme
    ignore
    (domainOrWWW "ctan.org")
    ignore
    (do
        (p:_) <- stripPrefixP "/pkg"
        pure p
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdCTAN p)

-- | @"freedesktop-gitlab": "https://gitlab.freedesktop.org/{project}.git/"@
freedesktopParser :: URIParser
freedesktopParser = URIParser
    (choice [httpScheme, string "git:"])
    ignore
    (domainOrWWW "gitlab.freedesktop.org")
    ignore
    (do
        (u:r:_) <- stripPrefixP "/"
        (u,) <$> gitPath r
    )
    ignore
    ignore
    (\_ _ _ _ (u,r) _ _ -> RemoteIdFreedesktop u r)

-- | @'gentoo': 'https://gitweb.gentoo.org/{project}.git/'@
gentooParser :: URIParser
gentooParser = URIParser
    httpScheme
    ignore
    (string "gitweb.gentoo.org")
    ignore
    (do
        (s:_) <- stripPrefixP "/"
        gitPath s
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdGentoo p)

-- | @'github': 'https://github.com/{project}'@
githubParser :: URIParser
githubParser = URIParser
    (choice [httpScheme, string "git:"])
    ignore
    (domainOrWWW "github.com")
    ignore
    (do
        (u:r:_) <- stripPrefixP "/"
        (u,) <$> gitPath r
    )
    ignore
    ignore
    (\_ _ _ _ (u,r) _ _ -> RemoteIdGithub u r)

-- | @'gitlab': 'https://gitlab.com/{project}'@
gitlabParser :: URIParser
gitlabParser = URIParser
    (choice [httpScheme, string "git:"])
    ignore
    (domainOrWWW "gitlab.com")
    ignore
    (do
        (u:r:_) <- stripPrefixP "/"
        (u,) <$> gitPath r
    )
    ignore
    ignore
    (\_ _ _ _ (u,r) _ _ -> RemoteIdGitlab u r)

-- | @"gnome-gitlab": "https://gitlab.gnome.org/{project}.git/"@
gnomeParser :: URIParser
gnomeParser = URIParser
    (choice [httpScheme, string "git:"])
    ignore
    (domainOrWWW "gitlab.gnome.org")
    ignore
    (do
        (u:r:_) <- stripPrefixP "/"
        (u,) <$> gitPath r
    )
    ignore
    ignore
    (\_ _ _ _ (u,r) _ _ -> RemoteIdGnome u r)

-- | @"kde-invent": "https://invent.kde.org/{project}"@
kdeParser :: URIParser
kdeParser = URIParser
    (choice [httpScheme, string "git:"])
    ignore
    (domainOrWWW "invent.kde.org")
    ignore
    (do
        (u:r:_) <- stripPrefixP "/"
        (u,) <$> gitPath r
    )
    ignore
    ignore
    (\_ _ _ _ (u,r) _ _ -> RemoteIdKDE u r)

-- | @'launchpad': 'https://launchpad.net/{project}'@
launchpadParser :: URIParser
launchpadParser = URIParser
    httpScheme
    ignore
    (domainOrWWW "launchpad.net")
    ignore
    (do
        (p:_) <- stripPrefixP "/"
        pure p
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdLaunchpad p)

-- | @'osdn': 'https://osdn.net/projects/{project}/'@
osdnParser :: URIParser
osdnParser = URIParser
    httpScheme
    ignore
    (domainOrWWW "osdn.net")
    ignore
    (do
        (p:_) <- stripPrefixP "/projects"
        pure p
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdOSDN p)

-- | @'pecl': 'https://pecl.php.net/package/{project}'@
peclParser :: URIParser
peclParser = URIParser
    httpScheme
    ignore
    (string "pecl.php.net")
    ignore
    (do
        (p:_) <- stripPrefixP "/package"
        pure p
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdPECL p)

-- | @'pypi': 'https://pypi.org/project/{project}/'@
pypiParser :: URIParser
pypiParser = URIParser
    httpScheme
    ignore
    (domainOrWWW "pypi.org")
    ignore
    (do
        (p:_) <- stripPrefixP "/project"
        pure p
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdPyPI p)

-- | @'rubygems': 'https://rubygems.org/gems/{project}'@
rubygemsParser :: URIParser
rubygemsParser = URIParser
    httpScheme
    ignore
    (domainOrWWW "rubygems.org")
    ignore
    (do
        (g:_) <- stripPrefixP "/gems"
        pure g
    )
    ignore
    ignore
    (\_ _ _ _ g _ _ -> RemoteIdRubygems g)

-- | @"savannah": "https://savannah.gnu.org/projects/{project}"@
savannahParser :: URIParser
savannahParser = URIParser
    httpScheme
    ignore
    (string "savannah.gnu.org")
    ignore
    (do
        (p:_) <- stripPrefixP "/projects"
        pure p
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdSavannah p)

-- | @"savannah-nongnu": "https://savannah.nongnu.org/projects/{project}"@
savannahNonGnuParser :: URIParser
savannahNonGnuParser = URIParser
    httpScheme
    ignore
    (string "savannah.nongnu.org")
    ignore
    (do
        (p:_) <- stripPrefixP "/projects"
        pure p
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdSavannahNonGNU p)

-- | @'sourceforge': 'https://sourceforge.net/projects/{project}/'@
sourceforgeParser :: URIParser
sourceforgeParser = URIParser
    httpScheme
    ignore
    (domainOrWWW "sourceforge.net")
    ignore
    (do
        (p:_) <- stripPrefixP "/projects"
        pure p
    )
    ignore
    ignore
    (\_ _ _ _ p _ _ -> RemoteIdSourceforge p)

-- | @"sourcehut": "https://sr.ht/{project}/"@
sourcehutParser :: URIParser
sourcehutParser = URIParser
    (choice [httpScheme, string "git:"])
    ignore
    (subdomain "sr.ht")
    ignore
    (do
        (u:r:_) <- stripPrefixP "/"
        (u,) <$> gitPath r
    )
    ignore
    ignore
    (\_ _ _ _ (u,r) _ _ -> RemoteIdSourcehut u r)

-- | @'vim': 'https://vim.org/scripts/script.php?script_id={project}'@
vimParser :: URIParser
vimParser = URIParser
    httpScheme
    ignore
    (domainOrWWW "vim.org")
    ignore
    (string "/scripts/script.php")
    (do
        _ <- char '?'
        ss <- sepBy1 (optionMaybe scriptParser) (char '&')
        (s:_) <- pure $ catMaybes ss -- The first successful 'scriptParser'
        pure s
    )
    ignore
    (\_ _ _ _ _ s _ -> RemoteIdVim s)
  where
    scriptParser :: Parser String
    scriptParser = string "script_id=" *> many1 (noneOf ['=','&','#'])

-- | Run a specified 'URIParser' with a string
--
--   Internally, uses 'parseURI' to create a 'URI', and then uses each
--   parser specified in 'URIParser' on a specific part of the uri. These
--   intermediate results are coalesced with the supplied 'mkRemoteId'.
runUriParser
  :: URIParser
  -> String
  -> Either ParseError RemoteId
runUriParser (URIParser {..}) = join . parseIt go
  where
    go :: Parser (Either ParseError RemoteId)
    go = do
        cs <- allChars
        case parseURI cs of
            Just (URI scheme (Just (URIAuth user regname port)) path query fragment) ->
                pure $ mkRemoteId
                    <$> parseIt schemeParser scheme
                    <*> parseIt userParser user
                    <*> parseIt regnameParser regname
                    <*> parseIt portParser port
                    <*> parseIt pathParser path
                    <*> parseIt queryParser query
                    <*> parseIt fragmentParser fragment
            _ -> fail $ "Could not parse as a URI: " ++ show cs

    parseIt :: Parser a -> String -> Either ParseError a
    parseIt p = parse p ""

-- | Convenience function for 'stripPrefix', which uses 'allChars' as the
--   target path. Throws a parse error if 'stripPrefix' fails.
stripPrefixP
    :: Path -- ^ The prefix path to strip
    -> Parser [String]
stripPrefixP pre = do
    targ <- allChars
    case stripPrefix pre targ of
        Just ps -> pure ps
        Nothing -> fail $ "Path prefix does not match: \n"
            ++ "pre = " ++ show pre ++ "\n"
            ++ "targ = " ++ show targ ++ "\n"
            ++ "L.stripPrefix " ++ show (splitDirectories pre)
                ++ " " ++ show (splitDirectories targ) ++ " = Nothing"

-- | Strips a path of a prefix, then returns the result split along path
--   seperators. Returns 'Nothing' if the prefix path does not match the
--   beginning of the target path.
--
--   Examples:
--
--   >>> stripPrefix "/web/packages" "/web/packages/foo/"
--   Just ["foo"]
--
--   >>> stripPrefix "/" "/foo/bar"
--   Just ["foo","bar"]
--
--   >>> stripPrefix "" "/foo/bar"
--   Just ["/","foo","bar"]
--
--   >>> stripPrefix "/some/thing" "/something/else"
--   Nothing
stripPrefix
    :: Path -- ^ The prefix path to strip
    -> Path -- ^ The target path to strip from
    -> Maybe [String]
stripPrefix pre targ =
    L.stripPrefix
        (splitDirectories pre)
        (splitDirectories targ)

-- | Compares the input stream to the given domain. Parser succeeds if
--   either of the following is true:
--
--   * The input stream matches the target domain exactly
--   * The input stream matches the target domain prepended by @"www."@
--
--   e.g.
--
--   > domainOrWWW "github.com"
--
--   will match on @"github.com"@ or @"www.github.com"@
domainOrWWW
    :: Domain
    -> Parser ()
domainOrWWW targ = do
    sub <- allChars
    if sub == ("www." ++ targ) || sub == targ
        then pure ()
        else fail $ "domainOrWWW did not match:"
            ++ "\nsub: " ++ show sub
            ++ "\ntarg: " ++ show targ

-- | Remove any ".git" suffix from the specified string
gitPath
    :: String
    -> Parser String
gitPath p = case parse go "" p of
    Left e  -> fail $ show e -- Not ideal, but it works
    Right r -> pure r
  where
    go :: Parser String
    go = choice
        [ try $ manyTill anyChar (string ".git") <* eof
        , allChars
        ]

-- | Compares the input stream to the given domain. Parser succeeds if the
--   input stream is a subdomain of the target.
subdomain
    :: Domain -- ^ Target domain to match against
    -> Parser ()
subdomain targ = do
    sub <- allChars
    if targ `L.isSuffixOf` sub
        then pure ()
        else fail $ show sub ++ " is not a subdomain of " ++ show targ

ignore :: Parser ()
ignore = pure ()

-- | Matches either of the strings @"http:"@ or @"https:"@
httpScheme :: Parser String
httpScheme = choice
    [ try $ string "https:"
    , string "http:"
    ]

-- | Match on every character from the input stream
allChars :: Parser String
allChars = many anyChar

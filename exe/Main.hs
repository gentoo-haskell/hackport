{-# LANGUAGE ApplicativeDo #-}
{-# LANGUAGE CPP #-}

module Main (main) where

import Control.Applicative.Combinators
import qualified Control.Applicative.Combinators.NonEmpty as CNE
import Control.Monad
import Data.Bitraversable
import Data.Monoid (Endo(..))
import qualified Options.Applicative as Opt
import qualified Options.Applicative.Help as Opt
import qualified Text.Parsec as P
import qualified Text.Parsec.String as P
import qualified Distribution.Verbosity as V

import Status.Types (StatusDirection(..))

import Hackport.Env
import Hackport.Command.List
import Hackport.Command.MakeEbuild
import Hackport.Command.Update
import Hackport.Command.Status
import Hackport.Command.Merge

import Data.Version (showVersion)
import Distribution.Pretty (prettyShow)
import Distribution.Simple.Utils (cabalVersion)

import qualified Paths_cabal_install
import qualified Paths_hackport

main :: IO ()
main = join $ Opt.customExecParser prefs globalParser
  where
    prefs :: Opt.ParserPrefs
    prefs = Opt.prefs
      $  Opt.showHelpOnEmpty
      <> Opt.showHelpOnError
      <> Opt.noBacktrack
      <> Opt.helpLongEquals

-----------------------------------------------------------------------
-- Global
-----------------------------------------------------------------------

globalParser :: Opt.ParserInfo (IO ())
globalParser = Opt.info (Opt.helper <*> parser) infoMod
  where
    infoMod :: Opt.InfoMod (IO ())
    infoMod = Opt.progDesc
      $  "HackPort is an .ebuild generator from .cabal files "
      ++ "with hackage index support"

    parser :: Opt.Parser (IO ())
    parser =  versionParser
          <|> numericVersionParser
          <|> mainParser
      where
        versionParser :: Opt.Parser (IO ())
        versionParser = Opt.flag' printVersion
          $  Opt.short 'V'
          <> Opt.long "version"
          <> Opt.help "Print version information"
          where
            printVersion = putStrLn
              $  "hackport version "
              ++ showVersion Paths_hackport.version
              ++ "\nusing cabal-install "
              ++ showVersion Paths_cabal_install.version
              ++ " and the Cabal library version "
              ++ prettyShow cabalVersion

        numericVersionParser :: Opt.Parser (IO ())
        numericVersionParser = Opt.flag' printNumericVersion
          $  Opt.long "numeric-version"
          <> Opt.help "Print just the version number"
          where
            printNumericVersion = putStrLn $ showVersion Paths_hackport.version

        mainParser :: Opt.Parser (IO ())
        mainParser = do
          v <-  silentFlag
            <|> verbosityOption
            <|> (`appEndo` V.normal) . mconcat <$> many verbosityFlag

          op <- optional $ Opt.strOption
            $  Opt.short 'p'
            <> Opt.long "overlay-path"
            <> Opt.metavar "PATH"
            <> helpMulti
              [ "Override search path list where .hackport/ lives"
              , "(default list: ['.', paludis-ovls or emerge-ovls])"
              ]

          pp <- optional $ Opt.strOption
            $  Opt.long "portage-path"
            <> Opt.metavar "PATH"
            <> Opt.help "Override path to your portage tree"

          mode <- Opt.hsubparser
            $  subcmd "list"        listAction       listParser
            <> subcmd "make-ebuild" makeEbuildAction makeEbuildParser
            <> subcmd "update"      updateAction     updateParser
            <> subcmd "status"      statusAction     statusParser
            <> subcmd "merge"       mergeAction      mergeParser

          pure $ mode GlobalEnv
            { globalVerbosity = v
            , globalPathToOverlay = op
            , globalPathToPortage = pp
            }

          where
            subcmd
              :: String
              -> Env env ()
              -> Opt.ParserInfo env
              -> Opt.Mod Opt.CommandFields (GlobalEnv -> IO ())
            subcmd name env = Opt.command name . fmap (runEnv env)

            silentFlag :: Opt.Parser V.Verbosity
            silentFlag = Opt.flag' V.silent
                $  Opt.long "silent"
                <> Opt.help "Use the \"silent\" verbosity level"

            verbosityString :: P.Parser V.Verbosity
            verbosityString = choice
              [ V.silent <$ (P.try (P.string "0") <|> P.string "silent")
              , V.normal <$ (P.try (P.string "1") <|> P.string "normal")
              , V.verbose <$ (P.try (P.string "2") <|> P.string "verbose")
              , V.deafening <$ (P.try (P.string "3") <|> P.string "deafening")
              ] <* P.eof

            verbosityFlag :: Opt.Parser (Endo V.Verbosity)
            verbosityFlag = Opt.flag' (Endo V.moreVerbose)
              $  Opt.short 'v'
              <> Opt.help "Increase verbosity level"


            verbosityOption :: Opt.Parser V.Verbosity
            verbosityOption = Opt.option readm
                $  Opt.long "verbosity"
                <> helpMulti
                  [ "Set verbosity level "
                  , "(0-3 or (silent,normal,verbose,deafening))"
                  ]
              where
                readm :: Opt.ReadM V.Verbosity
                readm = Opt.eitherReader
                  $ either (Left . err) Right
                  . P.runParser verbosityString () "command line option"

                err :: P.ParseError -> String
                err _ = ($ "") $ Opt.displayS $ Opt.renderCompact $ Opt.extractChunk $ Opt.vsepChunks
                  [ Opt.stringChunk "Takes a number (0-3) or one of the following values:"
                  , Opt.tabulate
#if MIN_VERSION_optparse_applicative(0,17,0)
                    (Opt.prefColumns Opt.defaultPrefs)
#endif
                    =<< traverse (bitraverse Opt.stringChunk Opt.stringChunk)
                      [ ("silent", "No output")
                      , ("normal", "Default verbosity")
                      , ("verbose", "Increased verbosity")
                      , ("deafening", "Maximum verbosity")
                      ]
                  ]



-----------------------------------------------------------------------
-- List
-----------------------------------------------------------------------

listParser :: Opt.ParserInfo ListEnv
listParser = Opt.info parser infoMod
  where
    parser :: Opt.Parser ListEnv
    parser = do
      ps <- many $ Opt.strArgument $ Opt.metavar "PACKAGE"
      pure $ ListEnv ps

    infoMod :: Opt.InfoMod ListEnv
    infoMod = Opt.progDesc "List package versions matching pattern"

-----------------------------------------------------------------------
-- Make Ebuild
-----------------------------------------------------------------------

makeEbuildParser :: Opt.ParserInfo MakeEbuildEnv
makeEbuildParser = Opt.info parser infoMod
  where
    parser :: Opt.Parser MakeEbuildEnv
    parser = do
      cat <- Opt.strArgument $ Opt.metavar "<CATEGORY>"
      cabalFiles <- CNE.some $ Opt.strArgument $ Opt.metavar "[EBUILD FILE]"
      flags <- optional cabalFlagsParser
      notOnHackage <- Opt.switch
        $  Opt.long "not-on-hackage"
        <> Opt.help "Skip writing the hackage remote-id to metadata.xml"

      pure $ MakeEbuildEnv cat cabalFiles flags (not notOnHackage)

    infoMod :: Opt.InfoMod MakeEbuildEnv
    infoMod = Opt.progDesc "Make an ebuild from a .cabal file"

-----------------------------------------------------------------------
-- Update
-----------------------------------------------------------------------

updateParser :: Opt.ParserInfo ()
updateParser = Opt.info parser infoMod
  where
    parser :: Opt.Parser ()
    parser = pure ()

    infoMod :: Opt.InfoMod ()
    infoMod = Opt.progDesc "Update the local package database"

-----------------------------------------------------------------------
-- Status
-----------------------------------------------------------------------

statusParser :: Opt.ParserInfo StatusEnv
statusParser = Opt.info parser infoMod
  where
    parser :: Opt.Parser StatusEnv
    parser = do
      dir <- choice
        [ Opt.flag' OverlayToPortage
            $  Opt.long "to-portage"
            <> Opt.help "Print only packages likely to be interesting to move to the portage tree."
        , Opt.flag' HackageToOverlay
            $  Opt.long "from-hackage"
            <> Opt.help "Print only packages likely to be interesting to move from hackage tree."
        , pure PortagePlusOverlay
        ]

      pkgs <- many $ Opt.strArgument $ Opt.metavar "PACKAGE"

      pure StatusEnv
        { statusDirection = dir
        , statusPackages = pkgs
        }

    infoMod :: Opt.InfoMod StatusEnv
    infoMod = Opt.progDescDoc $ docMulti Opt.sep
      [ "Show up-to-date status against other repos"
      , "(hackage, ::gentoo)"
      ]

-----------------------------------------------------------------------
-- Merge
-----------------------------------------------------------------------

mergeParser :: Opt.ParserInfo MergeEnv
mergeParser = Opt.info parser infoMod
  where
    parser :: Opt.Parser MergeEnv
    parser = do
      flags <- optional $ cabalFlagsParser
      pkg <- Opt.strArgument $ Opt.metavar "PACKAGE"

      pure $ MergeEnv flags pkg

    infoMod :: Opt.InfoMod MergeEnv
    infoMod = Opt.progDesc "Make an ebuild out of hackage package"

-----------------------------------------------------------------------
-- Misc
-----------------------------------------------------------------------

helpMulti :: [String] -> Opt.Mod f a
helpMulti = Opt.helpDoc . docMulti Opt.sep

docMulti :: ([Opt.Doc] -> Opt.Doc) -> [String] -> Maybe Opt.Doc
docMulti f = Opt.unChunk . fmap f . traverse Opt.paragraph

cabalFlagsParser :: Opt.Parser String
cabalFlagsParser = Opt.strOption
        $  Opt.short 'f'
        <> Opt.long "flags"
        <> Opt.metavar "cabal_flags"
        <> helpMulti
          [ "Set cabal flags to certain state. Example:"
          , "--flags=-all_extensions"
          ]

module Main where

import Build_doctests (Component (..), Name (..), components)
import Test.DocTest (doctest)
import Data.Foldable (for_)
import GHC.IO.Encoding (setLocaleEncoding)
import System.Directory (getCurrentDirectory, makeAbsolute)
import System.Environment.Compat (unsetEnv)
import System.FilePath.Glob (glob)
import System.FilePath.Posix ((</>))
import System.IO (utf8)

main :: IO ()
main = do
    setLocaleEncoding utf8
    unsetEnv "GHC_ENVIRONMENT"
    pwd    <- getCurrentDirectory
    prefix <- makeAbsolute pwd

    for_ components $ \(Component name pkgs flags _) -> do
        putStrLn "----------"
        print name

        maybeSources <- case name of
            NameLib (Just "hackport-internal") -> Just <$> glob (prefix </> "src" </> "**/*.hs")
            NameExe "hackport"                 -> Just <$> glob (prefix </> "exe" </> "**/*.hs")
            _                                  -> fail $
                "Unexpected component: " ++ show name ++ "\n" ++
                "Please edit tests/doctests.hs to add sources for this component."

        for_ maybeSources $ \sources -> do
            let noWarnFlags = ["-Wwarn", "-Wno-default"]
            let args = flags ++ noWarnFlags ++ pkgs ++ sources
            putStrLn "Flags passed:"
            for_ args $ \a -> putStr "    " *> print a
            doctest args

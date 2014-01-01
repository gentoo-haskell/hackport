{-|
    Author      :  Sergei Trofimovich <slyfox@inbox.ru>
    Stability   :  experimental
    Portability :  haskell98

    Ungrouped utilitary stuff lays here until someone finds better place for it :]
-}

module Util
    ( run_cmd -- :: String -> IO (Maybe String)
    , split -- :: (a -> Bool) -> [a] -> [[a]]
    ) where

import System.IO
import System.Process
import System.Exit (ExitCode(..))

-- 'run_cmd' executes command and returns it's standard output
-- as 'String'.

run_cmd :: String -> IO (Maybe String)
run_cmd cmd = do (hI, hO, hE, hProcess) <- runInteractiveCommand cmd
                 hClose hI
                 output <- hGetContents hO
                 errors <- hGetContents hE -- TODO: propagate error to caller
                 length output `seq` hClose hO
                 length errors `seq` hClose hE

                 exitCode <- waitForProcess hProcess
                 return $ if (output == "" || exitCode /= ExitSuccess)
                          then Nothing
                          else Just output

split :: Eq a => (a -> Bool) -> [a] -> [[a]]
split _ [] = []
split p xs =
    case break p xs of
        (l, [])  -> [l]
        (l, _:r) -> l: split p r

{-|
    Author      :  Sergei Trofimovich <slyfox@inbox.ru>
    Stability   :  experimental
    Portability :  haskell98

    Ungrouped utilitary stuff lays here until someone finds better place for it :]
-}

module Util
    ( run_cmd -- :: String -> IO (Maybe String)
    ) where

import System.IO
import System.Process
import System.Exit

-- 'run_cmd' executes command and returns it's standard output
-- as 'String'.

run_cmd :: String -> IO (Maybe String)
run_cmd cmd = do (_hI, hO, _hE, hProcess) <- runInteractiveCommand cmd
                 output <- hGetContents hO
                 _exitCode <- waitForProcess hProcess
                 return $ if (output == "")
                          then Nothing
                          else Just output

module BlingBling where

import System.IO

-- what nobody needs but everyone wants...

-- FIXME: do something more fun here
forMbling lst f = do
    init <- hGetBuffering stdout
    hSetBuffering stdout NoBuffering
    xs <- mapM (\x -> putStr "." >> f x) lst
    putStrLn ""
    hSetBuffering stdout init
    return xs

module BlingBling where

import System.IO

-- what nobody needs but everyone wants...

-- FIXME: do something more fun here
forMbling :: [a] -> (a -> IO b) -> IO [b]
forMbling lst f = do
    origBuffering <- hGetBuffering stdout
    hSetBuffering stdout NoBuffering
    xs <- mapM (\x -> putStr "." >> f x) lst
    putStrLn ""
    hSetBuffering stdout origBuffering
    return xs

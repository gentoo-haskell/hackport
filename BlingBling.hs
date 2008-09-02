module BlingBling where

import qualified Progress

import System.IO
import Control.Exception as Exception (bracket)

-- what nobody needs but everyone wants...

-- FIXME: do something more fun here
forMbling :: [a] -> (a -> IO b) -> IO [b]
forMbling lst f =
  withBuffering stdout NoBuffering $ do
    xs <- mapM (\x -> putStr "." >> f x) lst
    putStrLn ""
    return xs

blingProgress :: Progress.Progress s String a -> IO a
blingProgress progress = do
  isTerm <- hIsTerminalDevice stdout
  if isTerm
    then canIHasTehBling
    else boring

  where
    boring = Progress.fold (flip const) fail return progress

    canIHasTehBling =
      withBuffering stdout NoBuffering $ do
        putChar (fst (char 0))
        result <- spin 0 progress
        putStr "\b \b"
        return result

    spin _ (Progress.Fail e) = fail e
    spin _ (Progress.Done r) = return r
    spin n (Progress.Step _ p) = do
        putStr ['\b', c]
        spin n' p
      where (c, n') = char n

    char :: Int -> (Char, Int)
    char 0 = ('/',  1)
    char 1 = ('-',  2)
    char 2 = ('\\', 3)
    char _ = ('|',  0)

withBuffering :: Handle -> BufferMode -> IO a -> IO a
withBuffering hnd mode action =
  Exception.bracket
    (hGetBuffering hnd) (hSetBuffering hnd)
    (\_ -> hSetBuffering hnd mode >> action)

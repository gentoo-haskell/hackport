module OverlayPortageDiff where

import Action
import AnsiColor
import Bash
import Portage
import P2

import Control.Arrow
import Control.Monad.Error
import Control.Monad.State

import qualified Data.List as List

import qualified Data.ByteString.Lazy.Char8 as L

import Data.Char
import qualified Data.Map as Map

import qualified Data.Traversable as T

overlayonly :: HPAction ()
overlayonly = do
    portdir <- getPortDir
    overlayPath <- getOverlayPath
    overlay <- liftIO $ readPortageTree overlayPath
    portage <- liftIO $ readPortagePackages portdir (Map.keys overlay)
    let (over, both, _port) = portageDiff overlay portage

    both' <- T.forM both $ mapM $ \e -> liftIO $ do
            -- can't fail, we know the ebuild exists in both portagedirs
            -- also, one of them is already bound to 'e'
            let (Just e1) = lookupEbuildWith portage (ePackage e) (comparing eVersion e)
                (Just e2) = lookupEbuildWith overlay (ePackage e) (comparing eVersion e)
            eq <- equals (eFilePath e1) (eFilePath e2)
            let ev = eVersion e
            return (ev, toColor (if eq then Green else Yellow) (show ev))

    let over' = Map.map (map ((id &&& (toColor Red . show)).eVersion)) over

        meld = Map.map (map snd) $ Map.unionWith (\a b -> List.sort (a++b)) both' over'

    liftIO $ putStrLn $ toColor Green "Green" ++ ": package in portage and overlay are the same"
    liftIO $ putStrLn $ toColor Yellow "Yellow" ++ ": package in portage and overlay differs"
    liftIO $ putStrLn $ toColor Red "Red" ++ ": package only exist in portage"
    forM_ (Map.toAscList meld) $ \(package, versions) -> liftIO $ do
        let (P c p) = package
        putStr $ c ++ '/':inColor White True Default p
        putStr " "
        forM_ versions (\v -> putStr v >> putChar ' ')
        putStrLn ""

toColor c t = inColor c False Default t

-- incomplete
portageDiff :: Portage -> Portage -> (Portage, Portage, Portage)
portageDiff p1 p2 = (in1, ins, in2)
    where ins = Map.filter (not . null) $
                    Map.intersectionWith (List.intersectBy $ comparing eVersion) p1 p2
          in1 = subtract p1 p2
          in2 = subtract p2 p1
          subtract x y = Map.filter (not . null) $
                       Map.differenceWith (\xs ys ->
                        let lst = foldr (List.deleteBy (comparing eVersion)) xs ys in
                        if null lst
                            then Nothing
                            else Just lst
                            ) x y

comparing f x y = f x == f y


-- | Compares two ebuilds, returns True if they are equal.
--   Disregards comments.
equals :: FilePath -> FilePath -> IO Bool
equals fp1 fp2 = do
    f1 <- L.readFile fp1
    f2 <- L.readFile fp2
    return (equal' f1 f2)

equal' :: L.ByteString -> L.ByteString -> Bool
equal' = comparing essence
    where
    essence = filter (not . isEmpty) . filter (not . isComment) . L.lines
    isComment = L.isPrefixOf (L.pack "#") . L.dropWhile isSpace
    isEmpty = L.null . L.dropWhile isSpace


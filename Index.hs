module Index where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Version (Version,parseVersion)
import Codec.Compression.GZip(decompress)
import Data.ByteString.Lazy.Char8(ByteString,unpack)
import Codec.Archive.Tar
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Package
import System.FilePath.Posix
import MaybeRead (readPMaybe)

type Index = [(String,String,GenericPackageDescription)]
type IndexMap = Map.Map String (Set.Set Version)

readIndex :: ByteString -> Index
readIndex str = do
    let unziped = decompress str
        untared = readTarArchive unziped
    entr <- archiveEntries untared
    case splitDirectories (tarFileName (entryHeader entr)) of
        [".",pkgname,vers,file] -> do
            let descr = case parsePackageDescription (unpack (entryData entr)) of
                    ParseOk _ pkg_desc -> pkg_desc
                    _  -> error $ "Couldn't read cabal file "++show file
            return (pkgname,vers,descr)
        _ -> fail "doesn't look like the proper path"

filterIndexByPV :: (String -> String -> Bool) -> Index -> Index
filterIndexByPV cond index = [ x | x@(p,v,_d) <- index, cond p v]

indexMapFromList :: [PackageIdentifier] -> IndexMap
indexMapFromList pids = Map.unionsWith Set.union $
    [ Map.singleton (pName name) (Set.singleton vers)
    | (PackageIdentifier {pkgName = name,pkgVersion = vers}) <- pids ]

pName                    :: PackageName -> String
pName (PackageName name) = name

mkPackage :: String -> PackageName
mkPackage = PackageName

indexToPackageIdentifier :: Index -> [PackageIdentifier]
indexToPackageIdentifier index = do
    (name,vers_str,_) <- index
    Just vers <- return $ readPMaybe parseVersion vers_str
    return $ PackageIdentifier {pkgName = PackageName name,pkgVersion = vers}

bestVersions :: IndexMap -> Map.Map String Version
bestVersions = Map.map Set.findMax

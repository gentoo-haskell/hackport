module Index where

import qualified Data.Set as Set
import qualified Data.Map as Map
import Data.Version (Version,parseVersion)
import Data.ByteString.Lazy.Char8(ByteString,unpack)
import qualified Codec.Archive.Tar       as Tar
import qualified Codec.Archive.Tar.Entry as Tar
import Codec.Archive.Tar.Entry(Entry(..), EntryContent(..))
import qualified Codec.Compression.GZip as GZip
import Distribution.PackageDescription
import Distribution.PackageDescription.Parse
import Distribution.Package
import System.FilePath.Posix
import MaybeRead (readPMaybe)

type Index = [(String,String,GenericPackageDescription)]
type IndexMap = Map.Map String (Set.Set Version)

readIndex :: ByteString -> Index
readIndex = createIndex
            . Tar.read . GZip.decompress
    where
      createIndex = Tar.foldEntries getEntry [] (const [])

getEntry         :: Entry -> Index -> Index
getEntry ent ind = case (splitDirectories . Tar.entryPath $ ent) of
                     [".",name,vers,file] -> case (cabalIndex ent) of
                                               (Just desc) -> (name,vers,desc) : ind
                                               _           -> error $ "Couldn't read cabal file " ++ (show file)
                     _                    -> ind

cabalIndex       :: Entry -> Maybe GenericPackageDescription
cabalIndex entry = case (Tar.entryContent entry) of
                     (NormalFile file _) -> case (parsePackageDescription . unpack $ file) of
                                              (ParseOk _ pkg_desc) -> Just pkg_desc
                                              _                    -> Nothing
                     _                   -> Nothing

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

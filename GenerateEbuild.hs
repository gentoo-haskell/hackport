module GenerateEbuild where

import Cabal2Ebuild

import Prelude hiding (catch)
import Distribution.Package
import Data.Version (showVersion)
import Network.URI
import System.Directory
import System.FilePath

mergeEbuild :: FilePath -> String -> EBuild -> IO ()
mergeEbuild target category ebuild = do
  let edir = target </> category </> name ebuild
  let epath = edir </> name ebuild ++"-"++ version ebuild <.> "ebuild"
  createDirectoryIfMissing True edir
  writeFile epath (showEBuild ebuild)

fixSrc :: URI -> PackageIdentifier -> EBuild -> EBuild
fixSrc serverURI p ebuild =
  ebuild {
    src_uri = show $ serverURI {
      uriPath = 
        (uriPath serverURI)
          </> pkgName p
          </> showVersion (pkgVersion p)
          </> pkgName p ++ "-" ++ showVersion (pkgVersion p)
          <.> "tar.gz"
      }
    }

{-hackage2ebuild ::
	(PackageIdentifier,String,String) ->	-- ^ the package
	HPAction EBuild
hackage2ebuild (pkg,tarball,sig) = do
	cfg <- getCfg
	tarballPath <- (if verify cfg then (do
		(tarPath,sigPath) <- downloadFileVerify (tmp cfg) tarball sig
		liftIO $ removeFile sigPath
		return tarPath) else (downloadTarball (tmp cfg) tarball))
		`sayNormal` ("Downloading tarball from '"++tarball++"' to '"++(tmp cfg)++"'... ",const "done.")
	tarType <- maybe (liftIO (removeFile tarballPath) >> throwError (UnknownCompression tarball)) return (tarballGetType tarballPath)
		`sayDebug` ("Guessing compression type of tarball... ",const "done.")
	filesInTarball <- tarballGetFiles (tarCommand cfg) tarballPath tarType
		`sayDebug` ("Getting list of files from tarball... ",const "done.")
		`catchError` (\x->liftIO (removeFile tarballPath) >> throwError x)
	(cabalDir,cabalName) <- maybe (throwError $ NoCabalFound tarball) return (findCabal filesInTarball)
		`sayDebug` ("Trying to find cabal file... ",\(dir,name)->"Found cabal file '"++name++"' in '"++dir++"'.")
	cabalFile <- tarballExtractFile tarballPath tarType (cabalDir++"/"++cabalName)
		`sayDebug` ("Extracting cabal file... ",const "done.")
	packageDescription <- case parseDescription cabalFile of
		ParseFailed err -> throwError $ CabalParseFailed cabalName (showError err)
		ParseOk descr -> return descr
		`sayDebug` ("Parsing '"++cabalName++"'... ",const "done.")
	let ebuild=cabal2ebuild (packageDescription{pkgUrl=tarball}) --we don't trust the cabal file as we just successfully downloaded the tarbal somewhere
	return ebuild {cabalPath=Just cabalDir}-}

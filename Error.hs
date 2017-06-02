{-# LANGUAGE DeriveDataTypeable #-}

module Error (HackPortError(..), throwEx, catchEx, hackPortShowError) where

import Data.Typeable
import Control.Exception.Extensible as EE

data HackPortError
    = ArgumentError String
    | ConnectionFailed String String
    | PackageNotFound String
    | InvalidTarballURL String String
    | InvalidSignatureURL String String
    | VerificationFailed String String
    | DownloadFailed String String
    | UnknownCompression String
    | UnpackingFailed String Int
    | NoCabalFound String
    | ExtractionFailed String String Int
    | CabalParseFailed String String
    | BashNotFound
    | BashError String
    | NoOverlay
    | MultipleOverlays [String]
    | UnknownVerbosityLevel String
    -- | WrongCacheVersion
    -- | InvalidCache
    | InvalidServer String
    deriving (Typeable, Show)

instance Exception HackPortError where

throwEx :: HackPortError -> IO a
throwEx = EE.throw

catchEx :: IO a -> (HackPortError -> IO a) -> IO a
catchEx = EE.catch

hackPortShowError :: HackPortError -> String
hackPortShowError err = case err of
    ArgumentError str -> "Argument error: "++str
    ConnectionFailed server reason -> "Connection to hackage server '"++server++"' failed: "++reason
    PackageNotFound pkg -> "Package '"++ pkg ++"' not found on server. Try 'hackport update'?"
    InvalidTarballURL url reason -> "Error while downloading tarball '"++url++"': "++reason
    InvalidSignatureURL url reason -> "Error while downloading signature '"++url++"': "++reason
    VerificationFailed file signature -> "Error while checking signature('"++signature++"') of '"++file++"'"
    DownloadFailed url reason -> "Error while downloading '"++url++"': "++reason
    UnknownCompression tarball -> "Couldn't guess compression type of '"++tarball++"'"
    UnpackingFailed tarball code -> "Unpacking '"++tarball++"' failed with exit code '"++show code++"'"
    NoCabalFound tarball -> "Tarball '"++tarball++"' doesn't contain a cabal file"
    ExtractionFailed tarball file code -> "Extracting '"++file++"' from '"++tarball++"' failed with '"++show code++"'"
    CabalParseFailed file reason -> "Error while parsing cabal file '"++file++"': "++reason
    BashNotFound -> "The 'bash' executable was not found. It is required to figure out your portage-overlay. If you don't want to install bash, use '-p path-to-overlay'"
    BashError str -> "Error while guessing your portage-overlay. Either set PORTDIR_OVERLAY in /etc/portage/make.conf or use '-p path-to-overlay'.\nThe error was: \""++str++"\""
    MultipleOverlays overlays -> "You have the following overlays available: '"++unwords overlays++"'. Please choose one by using 'hackport -p path-to-overlay' <command>"
    NoOverlay -> "You don't have PORTDIR_OVERLAY set in '/etc/portage/make.conf'. Please set it or use '-p path-to-overlay'"
    UnknownVerbosityLevel str -> "The verbosity level '"++str++"' is invalid. Please use debug,normal or silent"
    InvalidServer srv -> "Invalid server address, could not parse: " ++ srv
    --WrongCacheVersion -> "The version of the cache is too old. Please update the cache using 'hackport update'"
    --InvalidCache -> "Could not read the cache. Please ensure that it's up to date using 'hackport update'"

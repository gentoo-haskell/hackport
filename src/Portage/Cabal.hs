{-|
Module      : Portage.Cabal
License     : GPL-3+
Maintainer  : haskell@gentoo.org

Utilities to extract and manipulate information from a package's @.cabal@ file,
such as its license and dependencies.
-}
module Portage.Cabal
  ( convertLicense
  , partition_depends
  ) where

import qualified Data.List as L

import qualified Distribution.License as Cabal
import qualified Distribution.SPDX    as SPDX
import qualified Distribution.Package as Cabal
import qualified Distribution.Pretty  as Cabal

-- | Convert the Cabal 'SPDX.License' into the Gentoo format, as a 'String'.
--
-- Generally, if the license is one of the common free-software or
-- open-source licenses, 'convertLicense' should return the license
-- as a 'Right' 'String':
--
-- >>> convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.GPL_3_0_or_later)
-- Right "GPL-3+"
--
-- >>> convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.GPL_3_0_only)
-- Right "GPL-3"
--
-- If it is a more obscure license, this should alert the user by returning
-- a 'Left' 'String':
--
-- >>> convertLicense (SPDX.License $ SPDX.simpleLicenseExpression SPDX.EUPL_1_1)
-- Left ...
convertLicense :: SPDX.License -> Either String String
convertLicense l =
    case Cabal.licenseFromSPDX l of
        --  good ones
        Cabal.AGPL mv      -> Right $ "AGPL-" ++ case Cabal.prettyShow <$> mv of
                                                  Just "3"   -> "3"
                                                  Just "3.0" -> "3+"
                                                  _          -> "3" -- almost certainly version 3
        Cabal.GPL mv       -> Right $ "GPL-" ++ case Cabal.prettyShow <$> mv of
                                                  Just "2"   -> "2"
                                                  Just "2.0" -> "2+"
                                                  Just "3"   -> "3"
                                                  Just "3.0" -> "3+"
                                                  _          -> "2" -- possibly version 2
        Cabal.LGPL mv      -> Right $ "LGPL-" ++ case Cabal.prettyShow <$> mv of
                                                   Just "2"   -> "2"
                                                   -- Cabal can't handle 2.0+ properly
                                                   Just "2.0" -> "2"
                                                   Just "3"   -> "3"
                                                   Just "3.0" -> "3+"
                                                   _          -> "2.1" -- probably version 2.1
        Cabal.BSD2         -> Right "BSD-2"
        Cabal.BSD3         -> Right "BSD"
        Cabal.BSD4         -> Right "BSD-4"
        Cabal.PublicDomain -> Right "public-domain"
        Cabal.MIT          -> Right "MIT"
        Cabal.Apache mv    -> Right $ "Apache-" ++
                              maybe "1.1" Cabal.prettyShow mv -- probably version 1.1
        Cabal.ISC          -> Right "ISC"
        Cabal.MPL v        -> Right $ "MPL-" ++ Cabal.prettyShow v -- probably version 1.0
        -- bad ones
        Cabal.AllRightsReserved -> Left "EULA-style licence. Please pick it manually."
        Cabal.UnknownLicense _  -> Left "license unknown to cabal. Please pick it manually."
        Cabal.OtherLicense      -> Left "(Other) Please look at license file of package and pick it manually."
        Cabal.UnspecifiedLicense -> Left "(Unspecified) Please look at license file of package and pick it manually."

-- | Extract only the dependencies which are not bundled with @GHC@.
partition_depends :: [Cabal.PackageName] -> Cabal.PackageName -> [Cabal.Dependency] -> ([Cabal.Dependency], [Cabal.Dependency])
partition_depends ghc_package_names merged_cabal_pkg_name = L.partition (not . is_internal_depend)
    where is_internal_depend (Cabal.Dependency pn _vr _lib) = is_itself || is_ghc_package
              where is_itself = pn == merged_cabal_pkg_name
                    is_ghc_package = pn `elem` ghc_package_names

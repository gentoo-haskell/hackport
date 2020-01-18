module Portage.Cabal
  ( convertLicense
  , partition_depends
  ) where

import qualified Data.List as L

import qualified Distribution.License             as Cabal
import qualified Distribution.SPDX.License        as SPDX
import qualified Distribution.Package             as Cabal
import qualified Distribution.Text                as Cabal

-- map the cabal license type to the gentoo license string format
convertLicense :: SPDX.License -> Either String String
convertLicense l =
    case Cabal.licenseFromSPDX l of
        --  good ones
        Cabal.AGPL mv      -> Right $ "AGPL-" ++ (maybe "3" Cabal.display mv)  -- almost certainly version 3
        Cabal.GPL mv       -> Right $ "GPL-" ++ (maybe "2" Cabal.display mv)  -- almost certainly version 2
        Cabal.LGPL mv      -> Right $ "LGPL-" ++ (maybe "2.1" Cabal.display mv) -- probably version 2.1
        Cabal.BSD2         -> Right "BSD-2"
        Cabal.BSD3         -> Right "BSD"
        Cabal.BSD4         -> Right "BSD-4"
        Cabal.PublicDomain -> Right "public-domain"
        Cabal.MIT          -> Right "MIT"
        Cabal.Apache mv    -> Right $ "Apache-" ++ (maybe "1.1" Cabal.display mv) -- probably version 1.1
        Cabal.ISC          -> Right "ISC"
        Cabal.MPL v        -> Right $ "MPL-" ++ Cabal.display v -- probably version 1.0
        -- bad ones
        Cabal.AllRightsReserved -> Left "EULA-style licence. Please pick it manually."
        Cabal.UnknownLicense _  -> Left "license unknown to cabal. Please pick it manually."
        Cabal.OtherLicense      -> Left "(Other) Please look at license file of package and pick it manually."
        Cabal.UnspecifiedLicense -> Left "(Unspecified) Please look at license file of package and pick it manually."

partition_depends :: [Cabal.PackageName] -> Cabal.PackageName -> [Cabal.Dependency] -> ([Cabal.Dependency], [Cabal.Dependency])
partition_depends ghc_package_names merged_cabal_pkg_name = L.partition (not . is_internal_depend)
    where is_internal_depend (Cabal.Dependency pn _vr) = is_itself || is_ghc_package
              where is_itself = pn == merged_cabal_pkg_name
                    is_ghc_package = pn `elem` ghc_package_names

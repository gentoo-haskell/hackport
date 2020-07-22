module Main where

import Test.DocTest
-- Some modules fail to find module Paths_hackport, which is generated during building.
-- There is a way to make this work properly, but for now just test modules which
-- don't import Paths_hackport.
main = doctest [ "Portage/Cabal.hs"
               , "Portage/GHCCore.hs"
               , "Portage/PackageId.hs"
               , "Portage/Version.hs"
               , "Portage/Metadata.hs"
               ]

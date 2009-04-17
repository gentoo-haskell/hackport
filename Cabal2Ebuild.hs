-- A program for generating a Gentoo ebuild from a .cabal file
--
-- Author : Duncan Coutts <dcoutts@gentoo.org>
--
-- Created: 21 July 2005
--
-- Copyright (C) 2005 Duncan Coutts
--
-- This library is free software; you can redistribute it and/or
-- modify it under the terms of the GNU General Public License
-- as published by the Free Software Foundation; either version 2
-- of the License, or (at your option) any later version.
--
-- This library is distributed in the hope that it will be useful,
-- but WITHOUT ANY WARRANTY; without even the implied warranty of
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
-- General Public License for more details.
--
-- |
-- Maintainer  : haskell@gentoo.org
--
-- cabal2ebuild - a program for generating a Gentoo ebuild from a .cabal file
--
module Cabal2Ebuild
        (EBuild(..)
        ,Dependency(..)
        ,cabal2ebuild
        ,convertDependencies
        ,showEBuild) where

import qualified Distribution.PackageDescription as Cabal
                                                (PackageDescription(..))
import qualified Distribution.Package as Cabal  (PackageIdentifier(..)
                                                , Dependency(..)
                                                , PackageName(..))
import qualified Distribution.Version as Cabal  (VersionRange(..), versionBranch, Version)
import qualified Distribution.License as Cabal  (License(..))
import qualified Distribution.Text as Cabal  (display)
--import qualified Distribution.Compiler as Cabal (CompilerFlavor(..))

import Data.Char          (toLower,isUpper)
import Data.List          (intercalate, groupBy, partition, nub, sortBy, init, last)
import Data.Ord           (comparing)
import Data.Maybe         (catMaybes, fromJust)

data EBuild = EBuild {
    name :: String,
    version :: String,
    description :: String,
    homepage :: String,
    src_uri :: String,
    license :: String,
    slot :: String,
    keywords :: [String],
    iuse :: [String],
    depend :: [Dependency],
    features :: [String],
    -- comments on various fields for communicating stuff to the user
    licenseComments :: String,
    my_pn :: Maybe String --If the package's name contains upper-case
  }

type Package = String
newtype Version = Version [Int] deriving (Ord, Eq)
type UseFlag = String
data Dependency = AnyVersionOf               Package
                | ThisVersionOf      Version Package   -- =package-version
                | LaterVersionOf     Version Package   -- >package-version
                | EarlierVersionOf   Version Package   -- <package-version
                | OrLaterVersionOf   Version Package   -- >=package-version
                | OrEarlierVersionOf Version Package   -- <=package-version
                | DependEither Dependency Dependency   -- depend || depend
                | DependIfUse  UseFlag    Dependency   -- use? ( depend )
                | ThisMajorOf        Version Package   -- =package-version*
    deriving (Eq,Show)

ebuildTemplate :: EBuild
ebuildTemplate = EBuild {
    name = "foobar",
    version = "0.1",
    description = "",
    homepage = "",
    src_uri = "",
    license = "",
    slot = "0",
    keywords = ["~amd64","~x86"],
    iuse = [],
    depend = [],
    features = [],
    licenseComments = "",
    my_pn = Nothing
  }

cabal2ebuild :: Cabal.PackageDescription -> EBuild
cabal2ebuild pkg = ebuildTemplate {
    name        = map toLower cabalPkgName,
    version     = Cabal.display (Cabal.pkgVersion (Cabal.package pkg)),
    description = if null (Cabal.synopsis pkg) then Cabal.description pkg
                                               else Cabal.synopsis pkg,
    homepage        = Cabal.homepage pkg,
    src_uri         = Cabal.pkgUrl pkg,
    license         = convertLicense (Cabal.license pkg),
    licenseComments = licenseComment (Cabal.license pkg),
    depend          = defaultDepGHC
                    : (simplify_deps $
                         convertDependency (Cabal.Dependency (Cabal.PackageName "Cabal")
                                           (Cabal.descCabalVersion pkg))
                        ++ convertDependencies (Cabal.buildDepends pkg)),
    my_pn           = if any isUpper cabalPkgName then Just cabalPkgName else Nothing,
    features        = features ebuildTemplate
                   ++ (if null (Cabal.executables pkg) then [] else ["bin"])
                   ++ maybe [] (const (["lib","profile","haddock"]
                        ++ if cabalPkgName == "hscolour" then [] else ["hscolour"])
                        ) (Cabal.library pkg) -- hscolour can't colour its own sources
  } where
        cabalPkgName = Cabal.display $ Cabal.pkgName (Cabal.package pkg)

defaultDepGHC :: Dependency
defaultDepGHC     = OrLaterVersionOf (Version [6,6,1]) "dev-lang/ghc"

-- map the cabal license type to the gentoo license string format
convertLicense :: Cabal.License -> String
convertLicense Cabal.GPL          = "GPL-2"    -- almost certainly version 2
convertLicense Cabal.LGPL         = "LGPL-2.1" -- probably version 2.1
convertLicense Cabal.BSD3         = "BSD"      -- do we really not
convertLicense Cabal.BSD4         = "BSD"      -- distinguish between these?
convertLicense Cabal.PublicDomain = "public-domain"
convertLicense Cabal.AllRightsReserved = ""
convertLicense _                  = ""

licenseComment :: Cabal.License -> String
licenseComment Cabal.AllRightsReserved =
  "Note: packages without a license cannot be included in portage"
licenseComment Cabal.OtherLicense =
  "Fixme: \"OtherLicense\", please fill in manually"
licenseComment _ = ""

convertDependencies :: [Cabal.Dependency] -> [Dependency]
convertDependencies = concatMap convertDependency

convertDependency :: Cabal.Dependency -> [Dependency]
convertDependency (Cabal.Dependency pname@(Cabal.PackageName _name) _)
  | pname `elem` coreLibs = []      -- no explicit dep on core libs
convertDependency (Cabal.Dependency pname versionRange)
  = case versionRange of
      -- versionRange && versionRange
      (Cabal.IntersectVersionRanges v1 v2) -> [convert v1, convert v2]
      -- any other dep
      v                                    -> [convert v]

  where
    -- XXX: not always true, we should look properly for deps in the overlay
    -- to find the correct category
    ebuildName = "dev-haskell/" ++ map toLower (Cabal.display pname)

    convert :: Cabal.VersionRange -> Dependency
    convert Cabal.AnyVersion = AnyVersionOf ebuildName
    convert (Cabal.ThisVersion v) = ThisVersionOf (cabalVtoHPv v) ebuildName
    convert (Cabal.LaterVersion v)   = LaterVersionOf (cabalVtoHPv v) ebuildName
    convert (Cabal.EarlierVersion v) = EarlierVersionOf (cabalVtoHPv v) ebuildName
    convert (Cabal.UnionVersionRanges (Cabal.ThisVersion v1) (Cabal.LaterVersion v2))
      | v1 == v2 = OrLaterVersionOf (cabalVtoHPv v1) ebuildName
    convert (Cabal.UnionVersionRanges (Cabal.ThisVersion v1) (Cabal.EarlierVersion v2))
      | v1 == v2 = OrEarlierVersionOf (cabalVtoHPv v1) ebuildName
    convert (Cabal.UnionVersionRanges r1 r2)
      = DependEither (convert r1) (convert r2)

-- converts Cabal versiion type to hackopr version
cabalVtoHPv :: Cabal.Version -> Version
cabalVtoHPv = Version . Cabal.versionBranch

instance Show Version where
    show (Version v) = intercalate "." $ map show v

coreLibs :: [Cabal.PackageName]
coreLibs = map Cabal.PackageName
  ["array"
  ,"base"
--,"bytestring"   --already has ebuild
  ,"containers"
  ,"directory"
  --,"editline"
--,"filepath"     --already has ebuild
  ,"ghc"
  ,"ghc-prim"
  ,"haskell98"
  ,"hpc"          --has ebuild, but only in the overlay
  ,"integer"
  ,"old-locale"
  ,"old-time"
  ,"packedstring"
  ,"pretty"
  ,"process"
  ,"random"
  ,"readline"     --has ebuild, but only in the overlay
  ,"rts"
  ,"template-haskell"
  ,"unix"         --has ebuild, but only in the overlay
  ]

showEBuild :: EBuild -> String
showEBuild ebuild =
  ss "# Copyright 1999-2009 Gentoo Foundation". nl.
  ss "# Distributed under the terms of the GNU General Public License v2". nl.
  ss "# $Header:  $". nl.
  nl.
  ss "CABAL_FEATURES=". quote' (sepBy " " $ features ebuild). nl.
  ss "inherit haskell-cabal". nl.
  nl.
  (case my_pn ebuild of
     Nothing -> id
     Just pn -> ss "MY_PN=". quote pn. nl.
                ss "MY_P=". quote "${MY_PN}-${PV}". nl. nl).
  ss "DESCRIPTION=". quote (description ebuild). nl.
  ss "HOMEPAGE=". quote (homepage ebuild). nl.
  ss "SRC_URI=". quote (replaceVars (src_uri ebuild)).
     (if null (src_uri ebuild) then ss "\t#Fixme: please fill in manually"
         else id). nl.
  nl.
  ss "LICENSE=". quote (license ebuild).
     (if null (licenseComments ebuild) then id
         else ss "\t#". ss (licenseComments ebuild)). nl.
  ss "SLOT=". quote (slot ebuild). nl.
  ss "KEYWORDS=". quote' (sepBy " " $ keywords ebuild).nl.
  ss "IUSE=". quote' (sepBy ", " $ iuse ebuild). nl.
  nl.
  ss "DEPEND=". quote' (sepBy "\n\t\t" $ map showDepend $ depend ebuild). nl.
  (case my_pn ebuild of
     Nothing -> id
     Just _ -> nl. ss "S=". quote ("${WORKDIR}/${MY_P}"). nl)
  $ []
  where replaceVars = replaceCommonVars (name ebuild) (my_pn ebuild) (version ebuild)

showDepend :: Dependency -> Package
showDepend (AnyVersionOf         p) = p
showDepend (ThisVersionOf      v p) = "~" ++ p ++ "-" ++ show v
showDepend (LaterVersionOf     v p) = ">" ++ p ++ "-" ++ show v
showDepend (EarlierVersionOf   v p) = "<" ++ p ++ "-" ++ show v
showDepend (OrLaterVersionOf   v p) = ">=" ++ p ++ "-" ++ show v
showDepend (OrEarlierVersionOf v p) = "<=" ++ p ++ "-" ++ show v
showDepend (DependEither       dep1 dep2) = showDepend dep1
                                     ++ " || " ++ showDepend dep2
showDepend (DependIfUse        useflag dep@(DependEither _ _))
                                                = useflag ++ "? " ++ showDepend dep
showDepend (DependIfUse        useflag dep)  = useflag ++ "? ( " ++ showDepend dep++ " )"
showDepend (ThisMajorOf        v p) = "=" ++ p ++ "-" ++ show v ++ "*"

ss :: String -> String -> String
ss = showString

sc :: Char -> String -> String
sc = showChar

nl :: String -> String
nl = sc '\n'

quote :: String -> String -> String
quote str = sc '"'. ss str. sc '"'

quote' :: (String -> String) -> String -> String
quote' str = sc '"'. str. sc '"'

sepBy :: String -> [String] -> ShowS
sepBy _ []     = id
sepBy _ [x]    = ss x
sepBy s (x:xs) = ss x. ss s. sepBy s xs

getRestIfPrefix ::
	String ->	-- ^ the prefix
	String ->	-- ^ the string
	Maybe String
getRestIfPrefix (p:ps) (x:xs) = if p==x then getRestIfPrefix ps xs else Nothing
getRestIfPrefix [] rest = Just rest
getRestIfPrefix _ [] = Nothing

subStr ::
	String ->	-- ^ the search string
	String ->	-- ^ the string to be searched
	Maybe (String,String)  -- ^ Just (pre,post) if string is found
subStr sstr str = case getRestIfPrefix sstr str of
	Nothing -> if null str then Nothing else case subStr sstr (tail str) of
		Nothing -> Nothing
		Just (pre,post) -> Just (head str:pre,post)
	Just rest -> Just ([],rest)

replaceMultiVars ::
	[(String,String)] ->	-- ^ pairs of variable name and content
	String ->		-- ^ string to be searched
	String 			-- ^ the result
replaceMultiVars [] str = str
replaceMultiVars whole@((pname,cont):rest) str = case subStr cont str of
	Nothing -> replaceMultiVars rest str
	Just (pre,post) -> (replaceMultiVars rest pre)++pname++(replaceMultiVars whole post)

replaceCommonVars ::
	String ->	-- ^ PN
	Maybe String ->	-- ^ MYPN
	String ->	-- ^ PV
	String ->	-- ^ the string to be replaced
	String
replaceCommonVars pn mypn pv str
	= replaceMultiVars
		([("${P}",pn++"-"++pv)]
		++ maybe [] (\x->[("${MY_P}",x++"-"++pv)]) mypn
		++[("${PN}",pn)]
		++ maybe [] (\x->[("${MY_PN}",x)]) mypn
		++[("${PV}",pv)]) str


{- Here goes code for dependencies simplification -}

simplify_group_table :: Package ->
                        Maybe Version ->
                        Maybe Version ->
                        Maybe Version ->
                        Maybe Version ->
                        Maybe Version -> [Dependency]

-- simplify_group_table p ol       l        e        oe       exact
-- 1) trivial cases:
simplify_group_table    p Nothing  Nothing  Nothing  Nothing  Nothing  = error $ p ++ ": unsolvable constraints"
simplify_group_table    p (Just v) Nothing  Nothing  Nothing  Nothing  = [OrLaterVersionOf v p]
simplify_group_table    p Nothing  (Just v) Nothing  Nothing  Nothing  = [LaterVersionOf v p]
simplify_group_table    p Nothing  Nothing  (Just v) Nothing  Nothing  = [EarlierVersionOf v p]
simplify_group_table    p Nothing  Nothing  Nothing  (Just v) Nothing  = [OrEarlierVersionOf v p]
simplify_group_table    p Nothing  Nothing  Nothing  Nothing  (Just v) = [ThisVersionOf v p]

-- 2) simplification passes
simplify_group_table    p (Just (Version v1)) Nothing (Just (Version v2)) Nothing Nothing
    -- specian case: >=a-v.N a<v.(N+1)   => =a-v.N*
    | (init v1 == init v2) && (last v2 == last v1 + 1) = [ThisMajorOf (Version v1) p]
    | otherwise                                        = [OrLaterVersionOf (Version v1) p, EarlierVersionOf (Version v2) p]

-- TODO: simplify constraints of type: >=a-v1; > a-v2 and such

-- o3) therwise sink:
simplify_group_table    p (Just v)     l@(_)       e@(_)        oe@(_)       exact@(_) =   OrLaterVersionOf v p : simplify_group_table p Nothing  l e oe exact
simplify_group_table    p ol@(Nothing) (Just v)    e@(_)        oe@(_)       exact@(_) =     LaterVersionOf v p : simplify_group_table p ol Nothing e oe exact
simplify_group_table    p ol@(Nothing) l@(Nothing) (Just v)     oe@(_)       exact@(_) =   EarlierVersionOf v p : simplify_group_table p ol l Nothing oe exact
simplify_group_table    p ol@(Nothing) l@(Nothing) e@(Nothing)  (Just v)     exact@(_) = OrEarlierVersionOf v p : simplify_group_table p ol l e Nothing  exact
-- already defined earlier
-- simplify_group_table    p ol@(Nothing) l@(Nothing) e@(Nothing)  oe@(Nothing) (Just v)  = OrEarlierVersionOf v p : simplify_group_table p ol l e oe Nothing

--  >a-v1 >a-v2         => >a-(max v1 v2)
-- key idea: all constraints are enforcing constraints, so we can't get
-- more, than one interval.
simplify_group :: [Dependency] -> [Dependency]
simplify_group [dep@(AnyVersionOf _package)] = [dep]
simplify_group deps = simplify_group_table package
                                           min_or_later_v   -- >=
                                           min_later_v      -- >
                                           max_earlier_v    -- <
                                           max_or_earlier_v -- <=
                                           exact_this_v     -- ==
    where
          package = fromJust.getPackage $ head deps
          max_earlier_v    = safe_minimum $ map earlier_v deps
          max_or_earlier_v = safe_minimum $ map or_earlier_v deps
          min_later_v      = safe_maximum $ map later_v deps
          min_or_later_v   = safe_maximum $ map or_later_v deps
          exact_this_v     = case catMaybes (map this_v deps) of
                                  []  -> Nothing
                                  [v] -> Just v
                                  xs  -> error $ "too many exact versions:" ++ show xs
          --
          earlier_v (EarlierVersionOf v _p) = Just v
          earlier_v _                       = Nothing

          or_earlier_v (OrEarlierVersionOf v _p) = Just v
          or_earlier_v _                         = Nothing

          later_v (LaterVersionOf v _p) = Just v
          later_v _                     = Nothing

          or_later_v (OrLaterVersionOf v _p) = Just v
          or_later_v _                     = Nothing

          this_v (ThisVersionOf v  _p) = Just v
          this_v _                     = Nothing
          --
          safe_minimum xs = case catMaybes xs of
                                 [] -> Nothing
                                 xs' -> Just $ minimum xs'
          safe_maximum xs = case catMaybes xs of
                                 [] -> Nothing
                                 xs' -> Just $ maximum xs'

-- divide packages to groups (by package name), simplify groups, merge again
simplify_deps :: [Dependency] -> [Dependency]
simplify_deps deps = (concatMap (simplify_group.nub) $
                       groupBy cmpPkgName $
                         sortBy (comparing getPackageString) groupable)
                     ++ ungroupable
    where (ungroupable, groupable) = partition ((==Nothing).getPackage) deps
          --
          cmpPkgName p1 p2 = cmpMaybe (getPackage p1) (getPackage p2)
          cmpMaybe (Just p1) (Just p2) = p1 == p2
          cmpMaybe _         _         = False
          --
getPackage :: Dependency -> Maybe Package
getPackage (AnyVersionOf package) = Just package
getPackage (ThisVersionOf      _version package) = Just package
getPackage (LaterVersionOf     _version package) = Just package
getPackage (EarlierVersionOf   _version package) = Just package
getPackage (OrLaterVersionOf   _version package) = Just package
getPackage (OrEarlierVersionOf _version package) = Just package
getPackage (DependEither _dependency _Dependency) = Nothing
getPackage (DependIfUse  _useFlag    _Dependency) = Nothing
getPackage (ThisMajorOf        _version package) = Just package
--
getPackageString :: Dependency -> Package
getPackageString dep = maybe "" id $ getPackage dep

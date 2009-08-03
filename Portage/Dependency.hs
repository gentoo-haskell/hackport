module Portage.Dependency where

import Portage.Version
import Distribution.Text (display)

type Package = String
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

showDepend :: Dependency -> Package
showDepend (AnyVersionOf         p) = p
showDepend (ThisVersionOf      v p) = "~" ++ p ++ "-" ++ display v
showDepend (LaterVersionOf     v p) = ">" ++ p ++ "-" ++ display v
showDepend (EarlierVersionOf   v p) = "<" ++ p ++ "-" ++ display v
showDepend (OrLaterVersionOf   v p) = ">=" ++ p ++ "-" ++ display v
showDepend (OrEarlierVersionOf v p) = "<=" ++ p ++ "-" ++ display v
showDepend (DependEither       dep1 dep2) = showDepend dep1
                                     ++ " || " ++ showDepend dep2
showDepend (DependIfUse        useflag dep@(DependEither _ _))
                                                = useflag ++ "? " ++ showDepend dep
showDepend (DependIfUse        useflag dep)  = useflag ++ "? ( " ++ showDepend dep++ " )"
showDepend (ThisMajorOf        v p) = "=" ++ p ++ "-" ++ display v ++ "*"



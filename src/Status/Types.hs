module Status.Types
    ( FileStatus(..)
    , StatusDirection(..)
    , fromStatus
    ) where

import qualified Distribution.Simple.Utils as Cabal (comparing)

data StatusDirection
    = PortagePlusOverlay
    | OverlayToPortage
    | HackageToOverlay
    deriving Eq

data FileStatus a
        = Same a
        | Differs a a
        | OverlayOnly a
        | PortageOnly a
        | HackageOnly a
        deriving (Show,Eq)

instance Ord a => Ord (FileStatus a) where
    compare = Cabal.comparing fromStatus

instance Functor FileStatus where
    fmap f st =
        case st of
            Same a -> Same (f a)
            Differs a b -> Differs (f a) (f b)
            OverlayOnly a -> OverlayOnly (f a)
            PortageOnly a -> PortageOnly (f a)
            HackageOnly a -> HackageOnly (f a)

fromStatus :: FileStatus a -> a
fromStatus fs =
    case fs of
        Same a -> a
        Differs a _ -> a -- second status is lost
        OverlayOnly a -> a
        PortageOnly a -> a
        HackageOnly a -> a

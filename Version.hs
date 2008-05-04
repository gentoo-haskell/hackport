{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Version parser, according to Portage spec.

    Shamelessly borrowed from exi, somewhat modified

-}

module Version
  (
  Version(),
  Suffix(..),
  fromCabalVersion,
  showVersion,
  showSuffix,
  readVersion,
  parseVersion,
  getVersion,
  showRevPR
  ) where

import qualified Distribution.Version as Cabal
import Control.Monad
import Data.List
import Data.Maybe
import Text.ParserCombinators.Parsec

data Version   =  Version   [Int]         -- [1,42,3] ~= 1.42.3
                            (Maybe Char)  -- optional letter
                            [Suffix]
                            Int           -- revision, 0 means none
  deriving (Eq, Ord)

data Suffix   =  Alpha Int | Beta Int | Pre Int | RC Int | P_ Int
  deriving (Eq,Ord)

instance Show Version where
  show = showVersion

instance Show Suffix where
  show = showSuffix

showVersion :: Version -> String
showVersion (Version ver c suf rev)
        = showver ++ showc ++ concatMap showSuffix suf ++ showRev rev
  where showver  =  concat . intersperse "." . map show $ ver
        showc    =  maybe "" (:[]) c

showSuffix :: Suffix -> String
showSuffix (Alpha n)  =  "_alpha" ++ showPos n
showSuffix (Beta n)   =  "_beta"  ++ showPos n
showSuffix (Pre n)    =  "_pre"   ++ showPos n
showSuffix (RC n)     =  "_rc"    ++ showPos n
showSuffix (P_ n)     =  "_p"     ++ showPos n

showPos :: Int -> String
showPos 0 = ""
showPos n = show n

showRev :: Int -> String
showRev 0 = ""
showRev n = "-r" ++ show n

showRevPR :: Int -> String
showRevPR n = "r" ++ show n

fromCabalVersion :: Cabal.Version -> Version
fromCabalVersion (Cabal.Version nums _tags) =
    Version nums Nothing [] 0

-- | Function to call if you want to parse a version number.
getVersion :: String -> Version
getVersion ver = case parseVersion ver of
                   Left   _  ->
                     error $ "getVersion: version parse error '" ++ ver ++ "'"
                   Right  x  ->  x

parseVersion :: String -> Either ParseError Version
parseVersion = parse (readVersion >>= \x -> eof >> return x) "<version number>"

readVersion :: CharParser st Version
readVersion =  do  (ver,  _verr)  <-  readVer
                   (c,    _cr  )  <-  readC
                   (suf,  _sufr)  <-  readSufs
                   (rev,  _revr)  <-  readRev
                   return (Version ver c suf rev)

readVer      ::  CharParser st ([Int],          String)
readNum      ::  CharParser st (Int,            String)
readC        ::  CharParser st (Maybe Char,     String)
readSuf      ::  CharParser st (Suffix,         String)
readSufType  ::  CharParser st (Int -> Suffix,  String)
readSufs     ::  CharParser st ([Suffix],       String)
readRev      ::  CharParser st (Int,            String)

readVer      =  liftM ((\(x,y) -> (x, concat . intersperse "." $ y)) . unzip) (sepBy1 readNum (char '.'))
readNum      =  do  ds <- many1 digit
                    case read ds of
                      n -> return (n,ds)
readC        =  option (Nothing,  "")  (liftM (\x -> (Just x, [x])) letter)
readSuf      =  do  char '_'
                    (f,sr)  <-  readSufType
                    (n,nr)  <-  option (0, "") readNum
                    return (f n,"_" ++ sr ++ nr)

readSufType  =  choice [
                          liftM (\x -> (Alpha,  x)) (try $ string "alpha"),
                          liftM (\x -> (Beta,   x)) (try $ string "beta" ),
                          liftM (\x -> (Pre,    x)) (try $ string "pre"  ),
                          liftM (\x -> (RC,     x)) (try $ string "rc"   ),
                          liftM (\x -> (P_,     x)) (try $ string "p"    )
                       ]

readSufs     =  fmap ( ( \ (x,y) -> (x, concat y) ) . unzip ) (many readSuf)

readRev      =  option (0,        "")  (  do  rr      <- string "-r"
                                              (n,nr)  <-  readNum
                                              return (n,rr ++ nr)
                                       )

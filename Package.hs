{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Parser for categories and packages.

    Shamelessly borrowed from exi, somewhat modified
-}

module Package
  where

import Control.Monad
import Text.ParserCombinators.Parsec

import Version
import System.FilePath
-- import Portage.Utilities

type Category  =  String
type Package   =  String
type Slot      =  String

getPV xs      =  case parsePV xs of
                   Left   e  ->
                     error $ "getPV: cat/pkg-ver parse error '" ++ xs ++ "'\n" ++ show e
                   Right  x  ->  x

getP xs       =  case parseP xs of
                   Left   e  ->
                     error $ "getCatPkg: cat/pkg parse error '" ++ xs ++ "'\n" ++ show e
                   Right  x  ->  x

parsePV       =  parse (readPV >>= \x -> eof >> return x) "<cat/pkg-ver>"

readPV        =  do  cat         <-  readCat
                     char '/'
                     (pkg,mver)  <-  readPkgAndVer
                     case mver of
                       Nothing   ->  error "readPV: version expected"
                       Just ver  ->  return (cat, pkg, ver)

parseP        =  parse (readP >>= \x -> eof >> return x) "<cat/pkg>"

readP         =  do  cat         <-  readCat
                     char '/'
                     (pkg,mver)  <-  readPkgAndVer
                     case mver of
                       Nothing   ->  return (cat, pkg)
                       Just _    ->  error "readCatPkg: unexpected version"

readCat        ::  CharParser st Category
readPkgAndVer  ::  CharParser st (Package,Maybe Version)

readCat        =   many1 (letter <|> digit <|> oneOf "_-")
readPkgAndVer  =   do  pre    <-  many1 (letter <|> digit <|> oneOf "_+")
                       (p,v)  <-  option ("",Nothing)
                                            (do  char '-'
                                                 liftM (\v -> ("",Just v)) readVerOrFail
                                                   <|> liftM (\(p,v) -> ('-':p,v)) readPkgAndVer
                                            )
                       return (pre ++ p,v)

readVerOrFail  ::  CharParser st Version
readVerOrFail  =   try $
                   do  ver    <-  many1 (letter <|> digit <|> oneOf "_+.-")
                       case parseVersion ver of
                           Left   _  -> 
                             fail $ "version parse error"
                           Right  x  -> return x


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

type Category  =  String
type Package   =  String
type Slot      =  String

getPV :: String -> (Category, Package, Version)
getPV xs      =  case parsePV xs of
                   Left   e  ->
                     error $ "getPV: cat/pkg-ver parse error '" ++ xs ++ "'\n" ++ show e
                   Right  x  ->  x

getP :: String -> (Category, Package)
getP xs       =  case parseP xs of
                   Left   e  ->
                     error $ "getCatPkg: cat/pkg parse error '" ++ xs ++ "'\n" ++ show e
                   Right  x  ->  x

parsePV :: String -> Either ParseError (Category, Package, Version)
parsePV       =  parse (readPV >>= \x -> eof >> return x) "<cat/pkg-ver>"

readPV :: GenParser Char st (Category, Package, Version)
readPV        =  do  cat         <-  readCat
                     char '/'
                     (pkg,mver)  <-  readPkgAndVer
                     case mver of
                       Nothing   ->  fail "readPV: version expected"
                       Just ver  ->  return (cat, pkg, ver)

parseP :: String -> Either ParseError (Category, Package)
parseP        =  parse (readP >>= \x -> eof >> return x) "<cat/pkg>"

readP :: GenParser Char st (Category, Package)
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


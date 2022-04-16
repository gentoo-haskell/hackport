{-|
    Stability   :  provisional
    Portability :  haskell98

    Simplistic ANSI color support.
-}

module AnsiColor
  ( Color(..)
  , bold
  , inColor
  ) where

import Data.List

data Color  =  Black
            |  Red
            |  Green
            |  Yellow
            |  Blue
            |  Magenta
            |  Cyan
            |  White
            |  Default
  deriving Enum

esc :: [String] -> String
esc []  =  ""
esc xs  =  "\ESC[" ++ (intercalate ";" xs) ++ "m"

col :: Color -> Bool -> Color -> [String]
col fg bf bg = show (fromEnum fg + 30) : bf' [show (fromEnum bg + 40)]
  where bf' | bf         =  ("01" :)
            | otherwise  =  id

inColor :: Color -> Bool -> Color -> String -> String
inColor c bf bg txt = esc (col c bf bg) ++ txt ++ esc ["00"]

bold :: String -> String
bold = ansi "1" "22"
-- italic    = ansi "3" "23"
-- underline = ansi "4" "24"
-- inverse   = ansi "7" "27"

ansi :: String -> String -> String -> String
ansi on off txt = esc [on] ++ txt ++ esc [off]

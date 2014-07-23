{-|
    Stability   :  provisional
    Portability :  haskell98

    Simplistic ANSI color support.
-}

module AnsiColor
  where

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
esc xs  =  "\ESC[" ++ (concat . intersperse ";" $ xs) ++ "m"

col :: Color -> Bool -> Color -> [String]
col fg bf bg = show (fromEnum fg + 30) : bf' [show (fromEnum bg + 40)]
  where bf' | bf         =  ("01" :)
            | otherwise  =  id

inColor :: Color -> Bool -> Color -> String -> String
inColor c bf bg txt = esc (col c bf bg) ++ txt ++ esc ["00"]

bold, italic, underline, inverse :: String -> String
bold      = ansi "1" "22"
italic    = ansi "3" "23"
underline = ansi "4" "24"
inverse   = ansi "7" "27"

ansi :: String -> String -> String -> String
ansi on off txt = esc [on] ++ txt ++ esc [off]

{-
data Doc = Doc (Bool -> String -> String)

char chr = Doc (\_ c -> chr:c)

text str = Doc (\_ c -> str ++ c)

(Doc t) <> (Doc u) = Doc (\b c -> t b (u b c))

t <+> u = t <> char ' ' <> u

showDoc (Doc d) b = d b ""

color (Doc d) color = Doc (\ b c ->
    if not b
        then d b c
        else inColor color False Default (d b ""))
-}

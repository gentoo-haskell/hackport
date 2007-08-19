{-|
    Maintainer  :  Andres Loeh <kosmikus@gentoo.org>
    Stability   :  provisional
    Portability :  haskell98

    Simplistic ANSI color support.
-}

module AnsiColor
  where

-- import Portage.Config.Type
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

esc []  =  ""
esc xs  =  "\ESC[" ++ (concat . intersperse ";" $ xs) ++ "m"

col fg bf bg = show (fromEnum fg + 30) : bf' [show (fromEnum bg + 40)]
  where bf' | bf         =  ("01" :)
            | otherwise  =  id

inColor c bf bg txt = esc (col c bf bg) ++ txt ++ esc ["00"]

bold      = ansi "1" "22"
italic    = ansi "3" "23"
underline = ansi "4" "24"
inverse   = ansi "7" "27"

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

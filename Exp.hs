
{-

Copyright 2011 Mate J Kovacs

This file (Exp.hs) is part of iPoly.

iPoly is free software: you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation, either version 3 of the License, or
(at your option) any later version.

iPoly is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
GNU General Public License for more details.

You should have received a copy of the GNU General Public License
along with iPoly.  If not, see <http://www.gnu.org/licenses/>.

-}

module Exp ( Exp(..) ) where
import List
import Char

data Exp = Con Integer
         | Var String
         | Syn Char [Exp]
         deriving (Eq, Ord)

instance Show Exp where
  show = concat . intersperse "\n" . showTree "" ""

simplify :: Exp -> Exp
-- simplify (Sym '^' [])
simplify x = x

showTree :: String -> String -> Exp -> [String]
showTree hp tp (Con n) = [hp ++ show n, tp]
showTree hp tp (Var s) = [hp ++ s, tp]
showTree hp tp (Syn c es) = (fp ++ head ls) : (map (rp++) $ tail ls)
  where
    (hd, rt, tl, sn) = split4 es
    ls = concat $
         (map (showTree ".--" "|  ") hd) ++
         (map (showTree "|--" "|  ") rt) ++
         (map (showTree "`--" "   ") tl) ++
         (map (showTree ""    ""   ) sn)
    fp = hp ++ "{" ++ [c] ++ "}--"
    rp = tp ++ " " ++ " " ++ "   "

split4 :: [a] -> ([a],[a],[a],[a])
split4 [x] = ([],[],[],[x])
split4 xs = (head, rest, last, [])
  where
    (head, tail) = splitAt 1 xs
    n = length tail - 1
    (rest, last) = splitAt n tail

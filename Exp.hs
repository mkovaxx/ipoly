
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

module Exp ( Exp(..), simplify ) where
import List
import Char

data Exp = Con Integer
         | Var String
         | Syn Char [Exp]
         deriving (Eq, Ord)

instance Show Exp where
  show = concat . intersperse "\n" . showTree "" ""

prec :: Char -> Int
prec c = case c of
  '+' -> 1
  '-' -> 1
  '*' -> 2
  '~' -> 3
  '^' -> 4

render :: Int -> Exp -> String -> String
render _ (Con c) s = show c ++ s
render _ (Var n) s = n ++ s
render outer (Syn '~' [e]) s = "-" ++ left ++ (render inner e $ right ++ s)
  where
    inner = prec '~'
    (left,right) = if outer > inner then ("(",")") else ("","")
render outer (Syn op [e,f]) s = left ++ (recur e $ [op] ++ recur f (right ++ s))
  where
    inner = prec op
    recur = render inner
    (left,right) = if outer > inner then ("(",")") else ("","")

simplify :: Exp -> Exp
simplify (Syn '~' [x]) = case simplify x of
  (Con c) -> Con $ -c
  (Syn '~' [e]) -> e
  (Syn '-' [e,f]) -> Syn '-' [f,e]
  e -> e
simplify (Syn op [x,y]) = case (op, simplify x, simplify y) of
  ('+', Con c, Con d) -> Con $ c+d
  ('-', Con c, Con d) -> Con $ c-d
  ('*', Con c, Con d) -> Con $ c*d
  ('+', Con 0, e) -> e
  ('+', e, Con 0) -> e
  ('-', Con 0, e) -> e
  ('-', e, Con 0) -> e
  ('*', Con 1, e) -> e
  ('*', e, Con 1) -> e
  ('*', e, f) | e==f -> simplify $ Syn '^' [e,Con 2]  -- render V*V as V^2
  ('^', _, Con 0) -> Con 1
  ('^', e, Con 1) -> e
  ('^', Con 0, _) -> Con 0
  ('^', Con 1, _) -> Con 1
  ('^', Syn '^' [a,b], c) -> simplify $ Syn '*' [b,c]
  (op,e,f) -> Syn op [e,f]
simplify x = x

showTree :: String -> String -> Exp -> [String]
showTree hp tp (Con n) = [hp ++ "(" ++ show n ++ ")", tp]
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


{-

Copyright 2011 Mate J Kovacs

This file (Poly.hs) is part of iPoly.

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

module Poly where

import Char
import List
import Data.Map (Map)
import qualified Data.Map as Map

-- numeric mappings
elimZeros :: (Ord k, Num a) => Map k a -> Map k a
elimZeros = Map.filter (0/=)

zeros :: (Ord k) => Map k a
zeros = Map.empty

spike :: (Ord k, Num a) => k -> Map k a
spike x = Map.singleton x 1

build :: (Ord k, Num a) => [(k,a)] -> Map k a
build = elimZeros . ( Map.fromListWith (+) )

assoc :: (Ord k) => Map k a -> [(k,a)]
assoc = Map.toList

image :: (Ord k, Num a) => Map k a -> k -> a
image m x = Map.findWithDefault 0 x m

scale :: (Ord k, Num a) => a -> Map k a -> Map k a
scale y = elimZeros . ( Map.map (y*) )

accum :: (Ord k, Num a) => Map k a -> Map k a -> Map k a
accum a b = elimZeros $ Map.unionWith (+) a b

crossWith :: (Ord k1, Ord k2, Ord k, Num a) => (k1 -> k2 -> k) -> Map k1 a -> Map k2 a -> Map k a
crossWith f a b = elimZeros $ build [ (f t u, c*d) | (t,c) <- assoc a, (u,d) <- assoc b ]

-- ordered ring of polynomials
instance (Show n, Ord n, Ord x, Num x, Ord c, Num c) => Num (Poly n x c) where
  p + q = add p q
  p * q = mul p q
  negate p = con (-1) * p
  fromInteger n = con $ fromInteger n
  signum p = case compare p (con 0) of
               LT -> con (-1)
               EQ -> con 0
               GT -> con 1
  abs p = signum p * p

instance (Show n, Ord n, Ord x, Num x, Ord c, Num c) => Show (Poly n x c) where
  show (SSum ts) = if ts == zeros
                   then show 0
                   else concat $ intersperse "  +  " $ [ show c ++ " * " ++ show t | (t,c) <- assoc ts ]

  show (VSum ts) = if ts == zeros
                   then "Null"
                   else concat $ intersperse "  +  " $ [ show c ++ " * " ++ show t | (t,c) <- assoc ts ]

instance (Show n, Ord n, Num x) => Show (STerm n x) where
  show (SMul fs) = if fs == zeros
                   then show 1
                   else concat $ intersperse " * " $ [ show f ++ "^" ++ show x | (f,x) <- assoc fs ]

instance (Show n, Ord n, Num x) => Show (VTerm n x) where
  show (VMul v s) = show s ++ "*" ++ show v

instance (Show n) => Show (SAtom n) where
  show (SVar n) = filter ('"'/=) $ show n
  show (SDot a b) = "(" ++ show a ++ "*" ++ show b ++ ")"

instance (Show n) => Show (VAtom n) where
  show (VVar n) = filter ('"'/=) $ show n

showM :: (Show k, Ord k, Show a) => Map k a -> String
showM m = unlines [ show k ++ " -> " ++ show a | (k,a) <- assoc m ]

-- polynomial
data Poly name expn coef
  = SSum (Map (STerm name expn) coef)
  | VSum (Map (VTerm name expn) coef)
  deriving (Eq, Ord)

-- scalar term
data STerm name expn
  = SMul (Map (SAtom name) expn)
  deriving (Eq, Ord)

-- vector term
data VTerm name expn
  = VMul (VAtom name) (STerm name expn)
  deriving (Eq, Ord)

-- scalar atom
data SAtom name
  = SVar name
  | SDot (VAtom name) (VAtom name)
  deriving (Eq, Ord)

-- vector atom
data VAtom name
  = VVar name
  deriving (Eq, Ord)

-- primitives
con :: (Ord n, Ord x, Num x, Num c) => c -> Poly n x c
con c = SSum $ scale c $ spike $ SMul zeros

sca :: (Ord n, Ord x, Num x, Num c) => n -> Poly n x c
sca n = SSum $ spike $ SMul $ spike $ SVar n

vec :: (Ord n, Ord x, Num x, Num c) => n -> Poly n x c
vec n = VSum $ spike $ VMul (VVar n) (SMul $ zeros)

-- operations
add :: (Ord n, Ord x, Num c) => Poly n x c -> Poly n x c -> Poly n x c
add (SSum ts) (SSum us) = SSum $ accum ts us
add (VSum ts) (VSum us) = VSum $ accum ts us

mul :: (Ord n, Ord x, Num x, Num c) => Poly n x c -> Poly n x c -> Poly n x c
mul (SSum ts) (SSum us) = SSum $ crossWith mulSS ts us
mul (SSum ts) (VSum us) = VSum $ crossWith mulSV ts us
mul (VSum ts) (SSum us) = VSum $ crossWith mulSV us ts
mul (VSum ts) (VSum us) = SSum $ crossWith mulVV ts us

mulSS :: (Ord n, Num x) => STerm n x -> STerm n x -> STerm n x
mulSS (SMul fs) (SMul gs) = SMul $ accum fs gs

mulSV :: (Ord n, Num x) => STerm n x -> VTerm n x -> VTerm n x
mulSV t (VMul v u) = VMul v $ mulSS t u

mulVV :: (Ord n, Num x) => VTerm n x -> VTerm n x -> STerm n x
mulVV (VMul v t) (VMul w u) = mulSS (mulSS t u) ( SMul $ spike $ dotVW )
  where
    dotVW = if v < w then SDot v w else SDot w v

-- collect
collect :: (Show n, Ord n, Ord x, Num x, Ord c, Num c) => n -> Poly n x c -> Map x (Poly n x c)
collect n (SSum ts) = build $ map (factorS n) $ assoc ts
collect n (VSum ts) = build $ map (factorV n) $ assoc ts

factorS :: (Ord n, Ord x, Num x, Num c) => n -> (STerm n x, c) -> (x, Poly n x c)
factorS n (SMul t,c) = (image t (SVar n), SSum $ scale c $ spike $ SMul $ Map.delete (SVar n) t)

factorV :: (Show n, Ord n, Ord x, Num x, Ord c, Num c) => n -> (VTerm n x, c) -> (x, Poly n x c)
factorV n (VMul (VVar v) (SMul t), c) = (x, vec v * p) where (x,p) = factorS n (SMul t,c)

-- misc
deg :: (Num x, Ord x) => Poly n x c -> x
deg (SSum ts) = foldr (max . degS) 0 (Map.keys ts)
deg (VSum ts) = foldr (max . degV) 0 (Map.keys ts)

degS :: (Num x) => STerm n x -> x
degS (SMul fs) = Map.fold (+) 0 fs

degV :: (Num x) => VTerm n x -> x
degV (VMul v s) = 1 + degS s

constCoeff :: (Ord n, Ord x, Num c) => Poly n x c -> c
constCoeff (SSum ts) = image ts (SMul zeros)
constCoeff (VSum _ ) = 0

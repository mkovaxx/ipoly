
module Expr where

type Number = Rational
type Name = String

data Expr = Lit Number
          | Var Name
          | Add Expr Expr
          | Sub Expr Expr
          | Neg Expr
          | Mul Expr Expr
          | Div Expr Expr
          | Pow Expr Expr
          | Dot Expr Expr
          deriving Show

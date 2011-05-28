
{-

Copyright 2011 Mate J Kovacs

This file (Main.hs) is part of iPoly.

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

module Main where
import System.Console.CmdArgs
import Control.Monad.Error
import Data.Char

import Tokens
import Parser
import Exp
import Poly

-- conversions

-- augment token list
augment :: [Token] -> [Token]
augment (t:u:ts) = [t] ++ ins ++ augment (u:ts)
  where
    ins = if mul then [Whatever '*'] else []
    mul = case (t,u) of
      (Variable _, Variable _) -> True
      (Constant _, Variable _) -> True
      (Variable _, Constant _) -> True
      (Variable _, Whatever '(') -> True
      (Whatever ')', Variable _) -> True
      (Constant _, Whatever '(') -> True
      (Whatever ')', Constant _) -> True
      _ -> False
augment x = x

-- build polynomial
fromExp :: Exp -> Either String (Poly String Integer Integer)
fromExp (Con n) = return $ con n
fromExp (Var s) = return $ if isUpper (head s) then vec s else sca s
fromExp (Syn '~' [e]) = liftM negate $ fromExp e
fromExp (Syn opr [e,f]) = do
  a <- fromExp e
  b <- fromExp f
  case (opr, a, b) of
    ('+', SSum _, SSum _) -> return $ a + b
    ('+', VSum _, VSum _) -> return $ a + b
    ('+', SSum _, VSum _) -> throwError $ "scalar + vector"
    ('+', VSum _, SSum _) -> throwError $ "vector + scalar"
    
    ('-', SSum _, SSum _) -> return $ a - b
    ('-', VSum _, VSum _) -> return $ a - b
    ('-', SSum _, VSum _) -> throwError $ "scalar - vector"
    ('-', VSum _, SSum _) -> throwError $ "vector - scalar"
    
    ('*', _, _) -> return $ a * b
    
    ('^', _, _) -> if deg b == 0 then return $ a ^ constCoeff b else throwError $ "constant exponent needed"
    
    _   -> throwError $ "unsupported operator: '" ++ [opr] ++ "'"

toExp :: (Poly String Integer Integer) -> Exp
toExp (SSum ts) = Con 0
toExp (VSum ts) = Con 0

-- main logic

data Opts = Opts {expr :: String, targ :: String}
          deriving (Show, Data, Typeable)

getOpts = cmdArgs $ Opts
     { expr = def  &=  args  &=  typ "Expression"
     , targ = def  &=  name "targ"  &=  typ "String"  &=  help "Target variable"
     }  &=  program "svex"  &=  summary "Short summary goes here"  &=  helpArg [name "h"]

--

main = do
  x <- getOpts
  print x
  let tree = parse $ expr x
  print tree
  case fromExp tree of
    Left text -> putStr $ "ERROR: " ++ text
    Right poly -> let coll = collect (targ x) poly in putStr $ showM coll

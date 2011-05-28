
{-

Copyright 2011 Mate J Kovacs

This file (Parser.y) is part of iPoly.

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

{
module Parser ( parseTokens, parse ) where
import Tokens
import Exp
}

%name parseTokens
%tokentype { Token }
%error { parseError }

%token
  con     { Constant $$ }
  var     { Variable $$ }
  '+'     { Whatever '+' }
  '-'     { Whatever '-' }
  '*'     { Whatever '*' }
  '/'     { Whatever '/' }
  '^'     { Whatever '^' }
  '('     { Whatever '(' }
  ')'     { Whatever ')' }

%left '-' '+'
%left '/' '*'
%left NEG
%right '^'

%%

Exp : con                 { Con $1 }
    | var                 { Var $1 }
    | '(' Exp ')'         { $2 }
    | '-' Exp %prec NEG   { Syn '~' [$2] }
    | Exp '+' Exp         { Syn '+' [$1,$3] }
    | Exp '-' Exp         { Syn '-' [$1,$3] }
    | Exp '*' Exp         { Syn '*' [$1,$3] }
    | Exp '/' Exp         { Syn '/' [$1,$3] }
    | Exp '^' Exp         { Syn '^' [$1,$3] }

{
parseError _ = error "no parse"

parse = parseTokens . alexScanTokens
}
